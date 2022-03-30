module Main where

import Network.Wai.Handler.Warp
import Servant.Server
import ServantAPI (apiProxy, apiHandler)


main :: IO ()
main = do
    print "Running calculator app on port 8080"
    run 8080 $ serve apiProxy apiHandler
