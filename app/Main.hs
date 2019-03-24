module Main where

import System.CircuitBreaker (CircuitBreaker, withBreaker, initialBreakerState,
    CircuitBreakerConf, CircutBreakerError(..))

import Control.Monad(forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Concurrent(threadDelay)
import Control.Monad (when, forever)
import UnliftIO.Exception (catchAny)
import System.Random (randomIO)

testBreaker :: CircuitBreaker "Test" 1000 4
testBreaker = undefined

main :: IO ()
main = do
    cbConf <- initialBreakerState
    -- TODO the api for calling withBreaker kinda sucks. It'd be great to drop the 'undefined'

    forM_ [1..30] $ const . noteError . flip runReaderT cbConf $ withBreaker testBreaker computation
    threadDelay 5000000
    noteError . flip runReaderT cbConf $ withBreaker testBreaker computation
    where
        noteError action = print =<< catchAny action (const $ pure (Left Failed))



-- | This computation fails 50% of the time
computation :: ReaderT CircuitBreakerConf IO Int
computation = do
    shouldFail <- liftIO randomIO
    when shouldFail $ error "Failed"
    pure 42
