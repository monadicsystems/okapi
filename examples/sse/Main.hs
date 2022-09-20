{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Function ((&))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Okapi
import SlaveThread (fork)

main :: IO ()
main = do
  eventSource <- newEventSource
  threadID <- fork $
    forever $ do
      threadDelay 1000000
      time <- getPOSIXTime
      sendEvent eventSource $ Event Nothing Nothing $ pack $ show time
  run id (api eventSource)
  killThread threadID

type Okapi a = OkapiT IO a

api :: EventSource -> Okapi Response
api eventSource = do
  methodGET
  index <|> sse eventSource
  where
    index :: Okapi Response
    index = do
      pathEnd <|> pathParam @String `is` ""
      ok
        & setHeader ("Content-Type", "text/html")
        & setBodyFile "examples/sse/sse.html"
        & return

    sse :: EventSource -> Okapi Response
    sse eventSource = do
      pathParam @String `is` "events"
      ok
        & setBodyEventSource eventSource
        & return
