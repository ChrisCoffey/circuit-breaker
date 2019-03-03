module System.CircuitBreaker (
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Time.Clock (UTCTime)
import Numeric.Natural (Natural)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import UnliftIO.STM (TVar)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

newtype CircuitBreakerConf = CBConf {
    cbBreakers :: M.HashMap T.Text (TVar CircutState)
    }

-- | The current condition of a CircutBreaker may be Active, Testing, or Waiting for the
-- timeout to elapse.
data CBCondition
    = Active
    | Testing
    | Waiting
    deriving (Show, Eq, Ord, Bounded, Enum)

data CircutState = CircuitState {
      mostRecentError :: Maybe UTCTime
    , currentState :: CBCondition
    } deriving (Show)

-- | A constraint that allows pullng the circuit breaker
class HasCircuitConf env where
    getCircutState :: env -> CircuitBreakerConf

-- | Access a Circut Breaker's label at runtime
reifyCircuitBreaker :: forall label timeout. (KnownSymbol label, KnownNat timeout) =>
    CircuitBreaker label timeout
    -> (T.Text, Natural)
reifyCircuitBreaker _ = (l, t)
    where
        l = T.pack $ symbolVal (Proxy :: Proxy label)
        t = fromIntegral $ natVal (Proxy :: Proxy timeout)


data CircuitBreaker (a :: Symbol) (b :: Nat)

data CircutBreakerError
    = Failed
    deriving (Show, Eq, Ord)

withBreaker :: (Monad m, MonadUnliftIO m, MonadReader env m,
    HasCircuitConf env, KnownSymbol label, KnownNat timeout) =>
    CircuitBreaker label timeout
    -> m a
    -> m (Either CircutBreakerError a)
withBreaker _ action = undefined
