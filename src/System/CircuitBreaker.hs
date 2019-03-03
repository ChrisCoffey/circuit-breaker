module System.CircuitBreaker (
) where

import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Maybe (fromMaybe, isNothing)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Numeric.Natural (Natural)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import UnliftIO.Exception (bracketOnError, catchDeep)
import UnliftIO.MVar (MVar, putMVar, takeMVar, newMVar)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

newtype CircuitBreakerConf = CBConf {
    cbBreakers :: MVar (M.HashMap T.Text (MVar CircuitState))
    }

-- | The current condition of a CircutBreaker may be Active, Testing, or Waiting for the
-- timeout to elapse.
data CBCondition
    = Active
    | Testing
    | Waiting
    deriving (Show, Eq, Ord, Bounded, Enum)


data CircuitState = CircuitState {
      mostRecentError :: !(Maybe UTCTime)
    , currentState :: !CBCondition
    -- , currentlyTesting :: MVar Bool
    } deriving (Show)

-- | A constraint that allows pullng the circuit breaker
class HasCircuitConf env where
    getCircuitState :: env -> CircuitBreakerConf

-- | Access a Circut Breaker's label at runtime
reifyCircuitBreaker :: forall label timeout. (KnownSymbol label, KnownNat timeout) =>
    CircuitBreaker label timeout
    -> (T.Text, Natural)
reifyCircuitBreaker _ = (l, t)
    where
        l = T.pack $ symbolVal (Proxy :: Proxy label)
        t = fromIntegral $ natVal (Proxy :: Proxy timeout)

-- | The definition of a particular circuit breaker.
data CircuitBreaker (a :: Symbol) (b :: Nat)

data CircutBreakerError
    = Failed
    | CircuitBreakerClosed T.Text
    deriving (Show, Eq, Ord)

data CircutAction
    = SkipClosed
    | Run UTCTime
    deriving (Eq, Show)

-- | Brackets a computation with short-circuit logic. If an uncaught error occurs, then the
-- circuit breaker opens, which causes all additional threads entering the wrapped code to fail
-- until the specified timeout has expired. Once the timeout expires, a single thread may enter
-- the protected region. If the call succeeds, then the circuit breaker allows all other traffic
-- through. Otherwise, it resets the timeout, after which the above algorithm repeats.
withBreaker :: (Monad m, MonadUnliftIO m, MonadReader env m,
    HasCircuitConf env, KnownSymbol label, KnownNat timeout) =>
    CircuitBreaker label timeout
    -> m a
    -> m (Either CircutBreakerError a)
withBreaker breakerDefinition action = do
    -- Get the current circut breaker
    breakerCell <- cbBreakers <$> asks getCircuitState
    breakers <- takeMVar breakerCell
    let (lbl, timeout) = reifyCircuitBreaker breakerDefinition
        mbs = lbl `M.lookup` breakers
    bs <- maybe (newMVar newBreaker) pure mbs

    -- If it happens to be the first time this block has been called, store the new breakers state
    if isNothing mbs
        then putMVar breakerCell $ M.insert lbl bs breakers
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
    bracketOnError (before bs) (onError bs) (during lbl bs)
    where
        (label, timeout) = reifyCircuitBreaker breakerDefinition

        -- Given the current state of the circuit breaker, determine what action should
        -- be taken during the body of the 'bracket'.
        --
        -- Condition rules:
        -- 1) If the circuit breaker is in 'Active' then pass the call on
        -- 2) If the circuit breaker is in 'Testing' then fail the call
        -- 3) If the circuit breaker is in 'Waiting' and the timeout has not elapsed then fail the call
        -- 4) If the circuit breaker is in 'Waiting' and the timeout has elapsed then convert to 'Testing' and try the call
        before bs = do
            now <- liftIO getCurrentTime
            cb <- takeMVar bs
            let threshold = addUTCTime (- (fromIntegral timeout)) now
                elapsed = fromMaybe False $ (threshold >) <$> mostRecentError cb
            putMVar bs cb
            case currentState cb of
                Waiting | elapsed -> do
                    putMVar bs $ cb {currentState = Testing}
                    pure $ Run now
                Active -> do
                    putMVar bs cb
                    pure $ Run now
                Waiting -> do
                    putMVar bs cb
                    pure SkipClosed
                Testing -> do
                    putMVar bs cb
                    pure SkipClosed

        during _ _ (Run _) =
            Right <$> action
        during lbl bs SkipClosed =
            pure . Left $ CircuitBreakerClosed lbl

        -- In the event of an uncaught error during the bracketed computation, flip the circuit breaker to 'Waiting'
        onError bs (Run ts) = do
            bs' <- takeMVar bs
            putMVar bs $ CircuitState {mostRecentError = Just ts, currentState = Waiting}


newBreaker :: CircuitState
newBreaker = CircuitState {
    mostRecentError = Nothing,
    currentState = Active
    }

testBreaker ::
    CircuitState
    -> CircuitState
testBreaker cb = cb {currentState = Testing}

failedTest ::
    UTCTime
    -> CircuitState
    -> CircuitState
failedTest errorTime cb = CircuitState {
    mostRecentError = Just errorTime
    , currentState = Waiting
    }

passedTest ::
    CircuitState
    -> CircuitState
passedTest cb = CircuitState {
    mostRecentError = Nothing,
    currentState = Active
    }
