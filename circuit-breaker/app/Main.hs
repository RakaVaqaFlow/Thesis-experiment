{-# LANGUAGE OverloadedStrings #-}

import System.Random
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Concurrent
import Data.Time.Clock (getCurrentTime)
import Lib 

app :: Application
app _ respond = do
    randomNum <- randomRIO (0, 99) :: IO Int
    if randomNum < 30
        then respond $ responseLBS status429 [] "Internal Server Error"
        else respond $ responseLBS status200 [] "Hello, World!"

main :: IO ()
main = do
    cb <- initState 1 5 20.0 10 75.0 10 True
    let wrappedApp = circuitBreakerMiddleware cb app
    run 8080 wrappedApp