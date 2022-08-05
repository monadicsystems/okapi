module Okapi.Event where
import Okapi.Internal.Types
import qualified Control.Concurrent.Chan.Unagi as Unagi

-- TODO: Use MonadOkapi
newEventSource :: IO EventSource
newEventSource = Unagi.newChan

sendValue :: ToSSE a => EventSource -> a -> IO ()
sendValue (inChan, _outChan) = Unagi.writeChan inChan . toSSE

sendEvent :: EventSource -> Event -> IO ()
sendEvent (inChan, _outChan) = Unagi.writeChan inChan
