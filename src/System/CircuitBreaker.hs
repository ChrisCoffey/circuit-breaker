module System.CircuitBreaker (
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Numeric.Natural (Natural)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import UnliftIO.Environment (bracketOnError)
import UnliftIO.STM (MVar)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

newtype CircuitBreakerConf = CBConf {
    cbBreakers :: M.HashMap T.Text (MVar CircuitState)
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
    breakers <- asks getCircuitState
    bs = undefiend :: CircuitState
    -- bracketOnError the action
    -- Read the current status & determine whether to perform the action
    --      0) Get the current time
    --      1) Try to read the MVar
    --      2) If it is Nothing, then there is a test underway & we're still closed
    --      3) If it is a Just, check if we're 'Waiting' & the timeout has not elapsed
    --          4) If the timeout has elapsed, enter 'Testing' state
    --          5) Otherwise, run the computation
    --      6) If the computation fails, set the error time and enter 'Waiting' state
    bracketOnError before during onError
    -- It'll be imortant to always keep the MVar full (unless testing) to make sure that it never deadlocks
    where
        (label, timeout) = reifyCircuitBreaker breakerDefinition

        before = do
            now <- liftIO getCurrentTime


        during

        onError

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
