import Lib
import Network.HTTP.Client        -- http-client
import Network.HTTP.Client.TLS    -- http-client-tls

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    circuitBreaker <- initState 5 30
    -- Use circuitBreaker with HTTP requests
