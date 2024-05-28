{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types (status500)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.STM
import Data.Time.Clock (getCurrentTime)
import Lib 

app :: Application
app _ respond = do
    threadDelay 500000 -- Simulate some work with 0.5 second delay
    respond $ responseLBS status200 [] "Hello, World!"

main :: IO ()
main = do
    cb <- initState 1 5 50.0 2 75.0 10 -- Initialize with arbitrary values for demo
    let wrappedApp = circuitBreakerMiddleware cb app
    run 8080 wrappedApp