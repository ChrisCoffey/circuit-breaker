module Main (main) where

import System.CircuitBreaker

import Control.Concurrent.MVar
import Data.Either
import Numeric.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "System.CircuitBreaker" [
      decrementProperties
    , evaluationProperties
    , transitionProperties
    ]

transitionProperties :: TestTree
transitionProperties = testGroup "transition guard" [
    testCase "Waiting -> SkipClosed iff errors remain" $ do
        bs <- newMVar $ CircuitState {errorCount = 1, currentState = Waiting}
        action <- breakerTransitionGuard bs (ET 1)
        action @=? SkipClosed
    , testCase "Respects the error threshold" $ do
        bs <- newMVar $ CircuitState {errorCount = 9, currentState = Waiting}
        action <- breakerTransitionGuard bs (ET 10)
        bs' <- readMVar bs
        action @=? Run
        currentState bs' @=? Testing
    , testCase "Testing is a serialized state" $ do
        bs <- newMVar $ CircuitState {errorCount = 0, currentState = Testing}
        action <- breakerTransitionGuard bs (ET 1)
        action @=? SkipClosed
    , testCase "Active always -> Run" $ do
        bs <- newMVar $ CircuitState {errorCount = 5, currentState = Active}
        action <- breakerTransitionGuard bs (ET 1)
        action @=? Run
    ]

evaluationProperties :: TestTree
evaluationProperties = testGroup "evaluation" [
    testProperty "never evaluates if SkipClosed is provided" $ \(CircState rawBS, ca) ->
        cover 35 (ca == SkipClosed) "Skip Closed" .
        cover 35 (ca == Run) "Run" . ioProperty $ do
            bs <- newMVar rawBS
            cell <- newMVar False
            res <- breakerTryPerformAction "test" (swapMVar cell True) bs ca
            cellV <- readMVar cell
            pure $ if isLeft res
                   then not cellV
                   else cellV
    , testProperty "always sets status to active if it runs" $ \(CircState rawBS, ca) ->
        cover 35 (ca == SkipClosed) "Skip Closed" .
        cover 35 (ca == Run ) "Run" . ioProperty $ do
            bs <- newMVar rawBS
            cell <- newMVar False
            res <- breakerTryPerformAction "test" (swapMVar cell True) bs ca
            cellV <- readMVar cell
            ba' <- readMVar bs
            pure $ if cellV
                   then currentState ba' == Active
                   else isLeft res
    ]

decrementProperties :: TestTree
decrementProperties = testGroup "error decrement" [
    testProperty "decrements, but not through zero" $ \(CircState rawBS) ->
        cover 5 (errorCount rawBS < 2) "Minimum Boundary" . ioProperty $ do
            bs <- newMVar rawBS
            decrementErrorCount bs
            val <- readMVar bs
            pure $ if errorCount rawBS == 0
                   then errorCount rawBS == errorCount val
                   else errorCount val == errorCount rawBS - 1

    , testProperty "never changes state" $ \(CircState rawBS) -> ioProperty $ do
        bs <- newMVar rawBS
        decrementErrorCount bs
        val <- readMVar bs
        pure $ currentState val == currentState rawBS

    , testProperty "Always >= 0" $ \(CircState rawBS) ->
        cover 2 (errorCount rawBS == 0) "Minimum Boundary" . ioProperty $ do
        bs <- newMVar rawBS
        decrementErrorCount bs
        val <- readMVar bs
        pure $ errorCount val >= 0
    ]

newtype CircState = CircState CircuitState deriving Show
instance Arbitrary CircState where
    arbitrary = do
        st <- elements [Active, Testing, Waiting]
        ec <- arbitrary
        pure . CircState $ CircuitState {
              errorCount = ec
            , currentState = st
            }

instance Arbitrary CircuitAction where
    arbitrary = elements [Run, SkipClosed]


instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Int)

