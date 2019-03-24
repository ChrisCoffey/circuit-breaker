# circuit-breaker
Circuit breakers are an error handling machine inspired by the circuit breakers used in electrical systems. Just
like their namesake, software circuit breakers prevent pushing traffic through a failing component
until it has recovered.

As of the current version, all `CircuitBreaker`s use a "leaky bucket" approach to backoffs.
In the definition of a circuit breaker, the first `Natural` argument is the number of milliseconds that need to pass before an error is expunged.
Coupled with an error threshold argument, this provides a surprising amount of flexibility for cirucit breakers in the wild, although you'll want to monitor and tune them over time.

### Using a circuit breaker
The following short example illustrates how to define a circuit breaker & use it.

```haskell
testBreaker :: CircuitBreaker "Test" 1000 4
testBreaker = undefined

main :: IO ()
main = do
    -- Initializes the empty storage for all circuit breakers
    cbConf <- initialBreakerState
    -- Perform a bunch of "work". Because we have a 50% failure rate and trigger the breaker after four
    -- errors, this will cause the breaker to disable additional calls.
    forM_ [1..30] $ const . noteError . flip runReaderT cbConf $ withBreaker testBreaker computation

    -- simulating a backoff long enough to decrement the accumulated errors.
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
```

The first thing to notice is that the `CircuitBreaker` definition exists entirely at the type level.
This guarantees that the definition of a particular `CircuitBreaker` can't somehow change at runtime.

All calls to `withBreaker` require a `CircuitBreakerConf` to be present in a reader environment.
`initialBreakerState` simply initializes an empty `CircuitBreakerConf` to save you the trouble of creating one yourself.

### Contributing
PRs and issues are welcome! Please let me know what you think could be improved or submit the patch yourself.

### License
MIT
