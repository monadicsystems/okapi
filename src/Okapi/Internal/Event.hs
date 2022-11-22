{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Internal.Event
  ( ToSSE (..),
    Event (..),
    EventSource (..),
    Chan (..),
    eventSourceAppUnagiChan,
    eventToServerEvent,
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Function as Function
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wai.EventSource as WAI

-- $serverSentEvents

class ToSSE a where
  toSSE :: a -> Event

data Event
  = Event
      { eventName :: Maybe Text.Text,
        eventID :: Maybe Text.Text,
        eventData :: LBS.ByteString
      }
  | CommentEvent LBS.ByteString
  | CloseEvent
  deriving (Show, Eq)

type EventSource = Chan Event

type Chan a = (Unagi.InChan a, Unagi.OutChan a)

eventSourceAppUnagiChan :: EventSource -> WAI.Application
eventSourceAppUnagiChan (inChan, _outChan) req sendResponse = do
  outChan <- IO.liftIO $ Unagi.dupChan inChan
  eventSourceAppIO (eventToServerEvent <$> Unagi.readChan outChan) req sendResponse
  where
    nl :: Builder.Builder
    nl = Builder.char7 '\n'

    field :: Builder.Builder -> Builder.Builder -> Builder.Builder
    field l b = l `mappend` b `mappend` nl

    eventSourceAppIO :: IO WAI.ServerEvent -> WAI.Application
    eventSourceAppIO src _ sendResponse =
      sendResponse $
        WAI.responseStream
          HTTP.status200
          [(HTTP.hContentType, "text/event-stream")]
          $ \sendChunk flush -> do
            flush
            Function.fix $ \loop -> do
              se <- src
              case eventToBuilder se of
                Nothing -> pure ()
                Just b -> sendChunk b >> flush >> loop
      where
        eventToBuilder :: WAI.ServerEvent -> Maybe Builder.Builder
        eventToBuilder (WAI.CommentEvent txt) = Just $ field commentField txt
          where
            commentField = Builder.char7 ':'
        eventToBuilder (WAI.RetryEvent n) = Just $ field retryField (Builder.string8 . show $ n)
          where
            retryField = Builder.string7 "retry:"
        eventToBuilder WAI.CloseEvent = Nothing
        eventToBuilder (WAI.ServerEvent n i d) =
          Just $
            mappend (name n (evid i $ evdata (mconcat d) nl)) nl
          where
            name Nothing = id
            name (Just n') = mappend (field nameField n')
            evid Nothing = id
            evid (Just i') = mappend (field idField i')
            evdata d' = mappend (field dataField d')

            nameField, idField, dataField :: Builder.Builder
            nameField = Builder.string7 "event:"
            idField = Builder.string7 "id:"
            dataField = Builder.string7 "data:"

eventToServerEvent :: Event -> WAI.ServerEvent
eventToServerEvent Event {..} =
  WAI.ServerEvent
    (Builder.byteString . Text.encodeUtf8 <$> eventName)
    (Builder.byteString . Text.encodeUtf8 <$> eventID)
    (Builder.word8 <$> LBS.unpack eventData)
eventToServerEvent (CommentEvent comment) = WAI.CommentEvent $ Builder.lazyByteString comment
eventToServerEvent CloseEvent = WAI.CloseEvent
