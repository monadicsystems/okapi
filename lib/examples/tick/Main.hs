{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad
import qualified Data.Binary.Builder as Builder
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.EventSource as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Okapi.App
import Okapi.Response
import qualified Web.HttpApiData as Web

ticker :: Chan.Chan Wai.ServerEvent -> Node '[]
ticker source =
    choice
        [ lit "connect" $ events source
        ]

tick source = do
    let event =
            Wai.ServerEvent
                { Wai.eventName = Nothing
                , Wai.eventId = Nothing
                , Wai.eventData = [Builder.putStringUtf8 "tick"]
                }
    forever do
        Concurrent.threadDelay (1 * (10 ^ 6))
        print "Sending"
        Chan.writeChan source event

main = do
    source <- Chan.newChan
    Concurrent.forkIO $ tick source
    Warp.run 8003
        . withDefault (ticker source)
        $ \_ resp -> resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."
