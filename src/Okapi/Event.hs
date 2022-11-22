module Okapi.Event
  ( newEventSource,
    sendValue,
    sendEvent,
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import Okapi.Internal.Event

newEventSource :: IO EventSource
newEventSource = Unagi.newChan

sendValue :: ToSSE a => EventSource -> a -> IO ()
sendValue (inChan, _outChan) = Unagi.writeChan inChan . toSSE

sendEvent :: EventSource -> Event -> IO ()
sendEvent (inChan, _outChan) = Unagi.writeChan inChan
