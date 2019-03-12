module Main where

import System.CircuitBreaker (CircuitBreaker, withBreaker, initialBreakerState,
    CircuitBreakerConf, CircutBreakerError(..))

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Concurrent(threadDelay)
import Control.Monad (when, forever)
import UnliftIO.Exception (catchAny)
import System.Random (randomIO)

type TestBreaker = CircuitBreaker "Test" 3 1

main :: IO ()
main = do
    cbConf <- initialBreakerState
    -- TODO the api for calling withBreaker kinda sucks. It'd be great to drop the 'undefined'
    res <- noteError . flip runReaderT cbConf $ withBreaker (undefined :: TestBreaker) computation
    res2 <- noteError . flip runReaderT cbConf $ withBreaker (undefined :: TestBreaker) computation
    print res
    print res2
    where
        noteError action = catchAny action (const $ pure (Left Failed))



-- | This computation fails 50% of the time
computation :: ReaderT CircuitBreakerConf IO Int
computation = do
    liftIO $ print "trying"
    shouldFail <- liftIO randomIO
    when shouldFail $ error "Failed"
    pure 42
