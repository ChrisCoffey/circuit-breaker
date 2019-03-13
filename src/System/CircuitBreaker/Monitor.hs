module System.CircuitBreaker.Monitor where

import System.CircuitBreaker (HasCircuitConf(..), CircuitBreakerConf(..),
    CircuitState(..))

import Control.Monad (forever, void)
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks, ReaderT, ask)
import Numeric.Natural (Natural)
import UnliftIO.Exception (SomeException)
import UnliftIO.MVar (MVar, putMVar, takeMVar, readMVar)
import UnliftIO.Concurrent(forkIO, threadDelay)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

newtype ErrorCallback m = OnError (SomeException -> m ())

data BreakerList
    = All
    | Some (S.HashSet T.Text)

data MonitorConfig m = MonitorConfig {
      errorHandler :: ErrorCallback m
    , delay :: Natural
    , breakers :: BreakerList
    }

class HasMonitorConf env m | env -> m where
    getMonitorConf :: env -> MonitorConfig m


-- | Run within a 'MoandReader'
runDefaultMonitorM :: (MonadUnliftIO m, MonadReader env m, HasCircuitConf env) =>
    m ()
runDefaultMonitorM = do
    cs <- asks getCircuitState
    runDefaultMonitor cs

-- | The deault circuit breaker monitor checks each breaker in succession, then waits for 750ms
-- before performing another pass.
runDefaultMonitor :: (MonadUnliftIO m) =>
    CircuitBreakerConf
    -> m ()
runDefaultMonitor =
    runMonitor defaultConf
    where
        defaultConf = MonitorConfig {
             errorHandler = OnError $ liftIO . print
           , delay = 750
           , breakers = All
            }


-- | Launches a monitor thread. With a configurable number of milliseconds between passes and the
-- breakers included in the pass, this provides more control over the performance of the monitors.
runMonitor :: MonadUnliftIO m =>
       MonitorConfig m
    -> CircuitBreakerConf
    -> m ()
runMonitor mc (CBConf cbBreakers) =
    void . forkIO . forever $ do
        threadDelay . fromIntegral $ delay mc
        breakerStorage <- readMVar cbBreakers
        let chosenBreakers = selectBreakers breakerStorage
        traverse decrementErrorCount chosenBreakers
    where
        selectBreakers = case breakers mc of
            All -> id
            Some bs -> M.filterWithKey (\k _ -> k `S.member` bs)

        decrementErrorCount breaker = do
            rawState <- takeMVar breaker
            let ec = errorCount rawState
            if ec == 0
            then putMVar breaker rawState
            else putMVar breaker $ rawState {errorCount = ec -1}

