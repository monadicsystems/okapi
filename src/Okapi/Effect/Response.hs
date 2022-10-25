{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Effect.Response where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Okapi.Event as Event
import qualified Okapi.State.Response as Response
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Response as Response
import qualified Web.Cookie as Web

class (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Response.StateM m) => ResponseM m

-- RESPONSE START --

setStatus :: ResponseM m => Response.Status -> m ()
setStatus status = Response.modify (\response -> response {Response.status = status})

setHeaders :: ResponseM m => Response.Headers -> m ()
setHeaders headers = Response.modify (\response -> response {Response.headers = headers})

setHeader :: ResponseM m => Response.Header -> m ()
setHeader header = do
  headers' <- Response.gets Response.headers
  Response.modify (\response -> response {Response.headers = update header headers'})
  where
    update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    update pair [] = [pair]
    update pair@(key, value) (pair'@(key', value') : ps) =
      if key == key'
        then pair : ps
        else pair' : update pair ps

setBody :: ResponseM m => Response.Body -> m ()
setBody body = Response.modify (\response -> response {Response.body = body})

setBodyRaw :: ResponseM m => LBS.ByteString -> m ()
setBodyRaw lbs = setBody (Response.Raw lbs)

setBodyFile :: ResponseM m => FilePath -> m ()
setBodyFile path = setBody (Response.File path) -- TODO: setHeader???

setBodyEventSource :: ResponseM m => Event.EventSource -> m ()
setBodyEventSource source = setBody (Response.EventSource source)

setPlaintext :: ResponseM m => Text.Text -> m ()
setPlaintext text = do
  setHeader ("Content-Type", "text/plain")
  setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

setHTML :: ResponseM m => LBS.ByteString -> m ()
setHTML html = do
  setHeader ("Content-Type", "text/html")
  setBody (Response.Raw html)

setJSON :: (Aeson.ToJSON a, ResponseM m) => a -> m ()
setJSON value = do
  setHeader ("Content-Type", "application/json")
  setBodyRaw (Aeson.encode value)

addHeader :: ResponseM m => Response.Header -> m ()
addHeader header = do
  headers <- Response.gets Response.headers
  Response.modify (\response -> response {Response.headers = header : headers})

addSetCookie :: ResponseM m => (BS.ByteString, BS.ByteString) -> m ()
addSetCookie (key, value) = do
  let setCookieValue =
        LBS.toStrict $
          Builder.toLazyByteString $
            Web.renderSetCookie $
              Web.defaultSetCookie -- TODO: Check that using default here is okay
                { Web.setCookieName = key,
                  Web.setCookieValue = value,
                  Web.setCookiePath = Just "/"
                }
  addHeader ("Set-Cookie", setCookieValue)

overwrite :: ResponseM m => LBS.ByteString -> m ()
overwrite = setBodyRaw

write :: (ResponseM m, Writeable a) => a -> m ()
write value = do
  body <- Response.gets Response.body
  setBodyRaw $ case body of
    Response.Raw raw -> raw <> toLBS value <> "\n"
    Response.File _ -> toLBS value
    Response.EventSource _ -> toLBS value

respond :: ResponseM m => Response.Response -> m ()
respond = Response.put

class Writeable a where
  toLBS :: a -> LBS.ByteString
  default toLBS :: Show a => a -> LBS.ByteString
  toLBS = LBS.fromStrict . Text.encodeUtf8 . Text.pack . Prelude.takeWhile ('"' /=) . Prelude.dropWhile ('"' ==) . show

instance Writeable Text.Text

instance Writeable LBS.ByteString where
  toLBS :: LBS.ByteString -> LBS.ByteString
  toLBS = id

instance Writeable Int

redirect :: ResponseM m => Response.Status -> Text.Text -> m ()
redirect status url =
  let headers = [("Location", Text.encodeUtf8 url)]
      body = Response.Raw ""
   in Response.put Response.Response {..}
