module Lib
    ( CircuitBreaker(..)
    , initState
    , circuitBreakerMiddleware
    ) where

import Control.Concurrent.STM
import Control.Exception
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime, diffUTCTime)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Conc (unsafeIOToSTM)
import Network.Wai
import Network.HTTP.Types

data CircuitState = Closed | Open | HalfOpen
    deriving (Eq, Show)

data CircuitBreakerOptions = CircuitBreakerOptions {
    timeout :: NominalDiffTime, -- таймаут запроса, по истечению которого он будет считаться неуспешным
    sleepWindow :: NominalDiffTime, -- сколько выключатель спит в open state
    errorRateThreshold :: Double, -- порог возможных ошибок
    permittedNumberOfCallsInHalfOpenState :: Integer, -- количество запросов которые проходят в целевой сервис в half-open state
    successRateThresholdInHalfOpenState :: Double, -- процент успешных запросов при котором cb переходит в состояние closed
    slidingWindowSize :: Integer -- размер окна учитываемых запросов
}

data CircuitBreaker = CircuitBreaker {
    state :: TVar CircuitState, -- состояние выключателя
    options :: CircuitBreakerOptions, -- опции выключателя
    slidingWindow :: TVar [Bool], -- буфер запросов
    position :: TVar Integer, -- позиция в буфере запросов
    lastAttemptedAt :: TVar UTCTime, -- время последнего запроса
    errorsInHalfOpen :: TVar Integer, -- кол-во ошибок в Half Open State
    successesInHalfOpen :: TVar Integer -- кол-во успешных запросов в Half Open State 
}

initState :: Integer -> Integer -> Double -> Integer -> Double -> Integer -> IO CircuitBreaker
initState timeout sleepWindow eRate pnmNum sRate slWinSize = do
    currentTime <- getCurrentTime
    stateVar <- newTVarIO Closed
    slidingWindowVar <- newTVarIO (replicate (fromIntegral slWinSize) True)
    positionVar <- newTVarIO 0
    lastAttemptedVar <- newTVarIO currentTime
    errorsInHalfOpenVar <- newTVarIO 0
    successesInHalfOpenVar <- newTVarIO 0
    return CircuitBreaker {
        state = stateVar,
        options = CircuitBreakerOptions (fromInteger timeout) (fromInteger sleepWindow) eRate pnmNum sRate slWinSize,
        slidingWindow = slidingWindowVar,
        position = positionVar,
        lastAttemptedAt = lastAttemptedVar,
        errorsInHalfOpen = errorsInHalfOpenVar,
        successesInHalfOpen = successesInHalfOpenVar
    }



circuitBreakerMiddleware :: CircuitBreaker -> Middleware
circuitBreakerMiddleware cb app req respond = do
    currentState <- readTVarIO (state cb)
    currentTime <- getCurrentTime
    lastAttempt <- readTVarIO (lastAttemptedAt cb)
    
    case currentState of
        Open -> if diffUTCTime currentTime lastAttempt > sleepWindow (options cb)
                  then do -- можно переходить в half-open state, поскольку прошло время сна
                    atomically $ writeTVar (state cb) HalfOpen 
                    resetHalfOpenValues cb
                    handleHalfOpenState cb app req respond
                  else respond $ responseLBS status429 [] (BSL.pack "Too Many Requests")
        HalfOpen -> handleHalfOpenState cb app req respond
        Closed -> handleClosedState cb app req respond

-- type Middleware = Application -> Application
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

handleHalfOpenState :: CircuitBreaker -> Application ->  Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleHalfOpenState cb app req respond = do
    let maxCalls = permittedNumberOfCallsInHalfOpenState (options cb)
    errors <- readTVarIO (errorsInHalfOpen cb)
    successes <- readTVarIO (successesInHalfOpen cb)
    if errors + successes < maxCalls
        then do
            result <- tryRequest cb app req
            case result of
                Left _ -> do
                    recordResult cb False
                    respond (responseLBS status503 [] (BSL.pack "Service Unavailable"))
                Right res -> do
                    recordResult cb True
                    res respond
        else do
            successRate <- calculateSuccessRate cb
            if successRate >= successRateThresholdInHalfOpenState (options cb)
                then atomically $ writeTVar (state cb) Closed
                else atomically $ writeTVar (state cb) Open >> resetFailureBuffer cb
            respond $ responseLBS status503 [] (BSL.pack "Service Unavailable")

handleClosedState :: CircuitBreaker -> Application -> Application
handleClosedState cb app req respond = do
    result <- tryRequest cb app req
    case result of
        Left _ -> do
            recordResult cb False
            errorRate <- calculateErrorRate cb
            if errorRate >= errorRateThreshold (options cb)
                then atomically $ writeTVar (state cb) Open >> resetFailureBuffer cb
                else return ()
            respond (responseLBS status503 [] (BSL.pack "Service Unavailable"))
        Right res -> do
            recordResult cb True
            res respond

tryRequest :: CircuitBreaker -> Application -> Request -> IO (Either SomeException ((Response -> IO ResponseReceived) -> IO ResponseReceived))
tryRequest cb app req = do
    currentTime <- getCurrentTime
    atomically $ writeTVar (lastAttemptedAt cb) currentTime
    try $ app req

recordResult :: CircuitBreaker -> Bool -> IO ()
recordResult cb success = do
    currentState <- readTVarIO (state cb)
    case currentState of 
        HalfOpen -> do
            if success
                then atomically $ do
                    successes <- readTVar (successesInHalfOpen cb)
                    writeTVar (successesInHalfOpen cb) (successes + 1)
                else atomically $ do
                    errors <- readTVar (errorsInHalfOpen cb)
                    writeTVar (errorsInHalfOpen cb) (errors + 1)
        Closed -> atomically $ do
            buffer <- readTVar (slidingWindow cb)
            pos <- readTVar (position cb)
            let newPos = (pos + 1) `mod` fromIntegral (slidingWindowSize $ options cb)
            let newBuffer = take (fromIntegral $ slidingWindowSize $ options cb) $ drop 1 buffer ++ [success]
            writeTVar (slidingWindow cb) newBuffer
            writeTVar (position cb) newPos

calculateErrorRate :: CircuitBreaker -> IO Double
calculateErrorRate cb = atomically $ do
    buffer <- readTVar (slidingWindow cb)
    let total = length buffer
        failures = length $ filter not buffer
    return $ (fromIntegral failures / fromIntegral total) * 100

calculateSuccessRate :: CircuitBreaker -> IO Double
calculateSuccessRate cb = atomically $ do
    buffer <- readTVar (slidingWindow cb)
    let total = length buffer
        successes = length $ filter id buffer
    return $ (fromIntegral successes / fromIntegral total) * 100

resetFailureBuffer :: CircuitBreaker -> STM ()
resetFailureBuffer cb = writeTVar (slidingWindow cb) (replicate (fromIntegral $ slidingWindowSize $ options cb) True)

resetHalfOpenValues :: CircuitBreaker -> IO ()
resetHalfOpenValues cb = atomically $ do
    writeTVar (errorsInHalfOpen cb) 0
    writeTVar (successesInHalfOpen cb) 0
