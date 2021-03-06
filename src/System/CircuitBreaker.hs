-- |
-- Module:          System.CircuitBreaker
-- Copyright:       (c) 2019 Chris Coffey
-- License:         MIT
-- Maintainer:      chris@foldl.io
-- Stability:       experimental
-- Portability:     portable
--
-- This is a batteries mostly-included circuit breaker library. It provides automatic backoff behavior by
-- preventing calls to a failing resource. For example, if a particular service is currently overloaded and
-- begins failing to respond to requests, then the circuit breaker will detect those errors and prevent
-- further calls from actually hitting the server in question. Once the service has had a chance to stabilize
-- the circuit breaker will allow a "testing" call to pass through. If it succeeds, then all other calls are
-- allowed to flow through. If it fails then it waits a bit longer before testing again.

module System.CircuitBreaker (
    CircuitBreaker,
    CircuitBreakerConf(..),
    withBreaker,

    CircutBreakerError(..),
    HasCircuitConf(..),
    CircuitState(..),
    CircuitAction(..),
    ErrorThreshold(..),
    CBCondition(..),

    initialBreakerState,
    -- | Exported for testing
    breakerTransitionGuard,
    breakerTryPerformAction,
    decrementErrorCount
) where

import Control.Monad (forever, void)
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks, ReaderT, ask)
import Data.Maybe (fromMaybe, isNothing)
import Numeric.Natural (Natural)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (bracketOnError, catchDeep)
import UnliftIO.MVar (MVar, putMVar, takeMVar, newMVar, swapMVar)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

newtype CircuitBreakerConf = CBConf {
    cbBreakers :: MVar (M.HashMap T.Text (MVar CircuitState))
    }

-- | Initialize the storage for the process' circuit breakers
initialBreakerState :: MonadUnliftIO m => m CircuitBreakerConf
initialBreakerState = do
    mv <- newMVar M.empty
    pure CBConf {cbBreakers = mv}

-- | The current condition of a CircutBreaker may be Active, Testing, or Waiting for the
-- timeout to elapse.
data CBCondition
    = Active
    | Testing
    | Waiting
    deriving (Show, Eq, Ord, Bounded, Enum)

data CircuitState = CircuitState {
      errorCount :: Natural
    , currentState :: !CBCondition
    } deriving (Show)

-- | A constraint that allows pullng the circuit breaker
class HasCircuitConf env where
    getCircuitState :: env -> CircuitBreakerConf

instance HasCircuitConf CircuitBreakerConf where
    getCircuitState = id

newtype ErrorThreshold = ET Natural
newtype DripFreq = DF Natural

-- | Access a Circut Breaker's label at runtime
reifyCircuitBreaker :: forall label df et. (KnownSymbol label, KnownNat df, KnownNat et) =>
    CircuitBreaker label df et
    -> (T.Text, DripFreq, ErrorThreshold)
reifyCircuitBreaker _ = (l, df, et)
    where
        l = T.pack $ symbolVal (Proxy :: Proxy label)
        df = DF . (* 1000) . fromIntegral $ natVal (Proxy :: Proxy df)
        et = ET . fromIntegral $ natVal (Proxy :: Proxy et)

-- | The definition of a particular circuit breaker
data CircuitBreaker (label :: Symbol) (dripFreq :: Nat) (errorThreshold :: Nat)

data CircutBreakerError
    = Failed
    | CircuitBreakerClosed T.Text
    deriving (Show, Eq, Ord)

data CircuitAction
    = SkipClosed
    | Run
    deriving (Eq, Show)


-- | Brackets a computation with short-circuit logic. If an uncaught error occurs, then the
-- circuit breaker opens, which causes all additional threads entering the wrapped code to fail
-- until the specified timeout has expired. Once the timeout expires, a single thread may enter
-- the protected region. If the call succeeds, then the circuit breaker allows all other traffic
-- through. Otherwise, it resets the timeout, after which the above algorithm repeats.
--
-- Important Note: This does not catch errors. If an IO error is thrown, it will bubble up from
-- this function. Internally, if the breaker is tripped, it will prevent further calls
withBreaker :: (KnownSymbol label, KnownNat df, KnownNat et, Monad m,
    MonadUnliftIO m, MonadReader env m, HasCircuitConf env) =>
    CircuitBreaker label df et
    -> m a
    -> m (Either CircutBreakerError a)
withBreaker breakerDefinition action = do
    -- Get the current circut breaker
    breakerCell <- cbBreakers <$> asks getCircuitState
    breakers <- takeMVar breakerCell
    let mbs = label `M.lookup` breakers
        newBreaker = CircuitState {
            errorCount = 0,
            currentState = Active
            }
    bs <- maybe (newMVar newBreaker) pure mbs

    -- If it happens to be the first time this block has been called, store the new breakers state.
    -- Also initialize a monitor thread to drip errors
    if isNothing mbs
        then do
            putMVar breakerCell $ M.insert label bs breakers
            monitor bs
        else  putMVar breakerCell breakers
    -- bracketOnError the action
    -- Read the current status & determine whether to perform the action
    --      0) Get the current time
    --      1) Try to read the MVar
    --      2) If it is Nothing, then there is a test underway & we're still closed
    --      3) If it is a Just, check if we're 'Waiting' & the timeout has not elapsed
    --          4) If the timeout has elapsed, enter 'Testing' state
    --          5) Otherwise, run the computation
    --      6) If the computation fails, set the error time and enter 'Waiting' state
    bracketOnError (breakerTransitionGuard bs (ET et)) (onError bs) (breakerTryPerformAction label action bs)
    where
        (label, DF dripFreq, ET et) = reifyCircuitBreaker breakerDefinition

        -- In the event of an uncaught error during the bracketed computation, flip the circuit breaker to 'Waiting'
        onError bs Run = do
            bs' <- takeMVar bs
            let ec' = 1 + errorCount bs'
                state = if ec' >= et then Waiting else Active
            putMVar bs $ CircuitState {errorCount = ec', currentState = state}

        -- Run a
        monitor bs =
            void . forkIO . forever $ do
                threadDelay $ fromIntegral dripFreq
                decrementErrorCount bs

-- | Given the current state of the circuit breaker, determine what action should
-- be taken during the body of the 'bracket'.
--
-- Condition rules:
-- 1) If the circuit breaker is in 'Active' then pass the call on
-- 2) If the circuit breaker is in 'Testing' then fail the call
-- 3) If the circuit breaker is in 'Waiting' and the timeout has not elapsed then fail the call
-- 4) If the circuit breaker is in 'Waiting' and the timeout has elapsed then convert to 'Testing' and try the call
breakerTransitionGuard :: (MonadUnliftIO m) =>
    MVar CircuitState
    -> ErrorThreshold
    -> m CircuitAction
breakerTransitionGuard bs (ET et) = do
    cb <- takeMVar bs
    let elapsed = errorCount cb < fromIntegral et
    case currentState cb of
        Waiting | elapsed -> do
            putMVar bs $ cb {currentState = Testing}
            pure Run
        Active -> do
            putMVar bs cb
            pure Run
        Waiting -> do
            putMVar bs cb
            pure SkipClosed
        Testing -> do
            putMVar bs cb
            pure SkipClosed

-- | Conditionally performs an action with in a circuit breaker, as determined by the current breaker state.
breakerTryPerformAction :: MonadUnliftIO m =>
    T.Text -- ^ label
    -> m a -- ^ Action to perform
    -> MVar CircuitState -- ^ Pointer to the breaker state
    -> CircuitAction
    -> m (Either CircutBreakerError a)
breakerTryPerformAction label _ _ SkipClosed =
    pure . Left $ CircuitBreakerClosed label
breakerTryPerformAction label action bs Run = do
    res <- Right <$> action
    bs' <- takeMVar bs
    putMVar bs (bs' {currentState = Active})
    pure res

-- | Decrements a counter
decrementErrorCount :: MonadUnliftIO m =>
    MVar CircuitState
    -> m ()
decrementErrorCount breaker = do
    rawState <- takeMVar breaker
    let ec = errorCount rawState
    if ec == 0
    then putMVar breaker rawState
    else putMVar breaker $ rawState {errorCount = ec -1}
