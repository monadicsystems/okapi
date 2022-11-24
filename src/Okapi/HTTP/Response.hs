{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.HTTP.Response
  ( Parser (..),
    Body,
    Status,
    Headers,
    Header,
    ok,
    noContent,
    notFound,
    forbidden,
    internalServerError,
    setStatus,
    setHeaders,
    setHeader,
    setBody,
    setBodyRaw,
    setBodyFile,
    setBodyEventSource,
    setPlaintext,
    setHTML,
    setJSON,
    addHeader,
    addSetCookie,
    overwrite,
    write,
    redirect,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Event as Event
import Okapi.Internal.Response
import qualified Web.Cookie as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

ok :: Response
ok =
  let status = 200
      headers = []
      body = Raw ""
   in Response {..}

noContent :: Response
noContent =
  let status = 204
      headers = []
      body = Raw ""
   in Response {..}

notFound :: Response
notFound =
  let status = 404
      headers = []
      body = Raw "Not Found"
   in Response {..}

forbidden :: Response
forbidden =
  let status = 403
      headers = []
      body = Raw "Forbidden"
   in Response {..}

internalServerError :: Response
internalServerError =
  let status = 500
      headers = []
      body = Raw "Internal Server Error"
   in Response {..}

-- response :: Parser m => m Response
-- response = get

-- RESPONSE START --

setStatus :: Parser m => Status -> m ()
setStatus status = modify (\response -> response {status = status})

setHeaders :: Parser m => Headers -> m ()
setHeaders headers = modify (\response -> response {headers = headers})

setHeader :: Parser m => Header -> m ()
setHeader header = do
  headers' <- gets headers
  modify (\response -> response {headers = update header headers'})
  where
    update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    update pair [] = [pair]
    update pair@(key, value) (pair'@(key', value') : ps) =
      if key == key'
        then pair : ps
        else pair' : update pair ps

setBody :: Parser m => Body -> m ()
setBody body = modify (\response -> response {body = body})

setBodyRaw :: Parser m => LBS.ByteString -> m ()
setBodyRaw lbs = setBody (Raw lbs)

setBodyFile :: Parser m => FilePath -> m ()
setBodyFile path = setBody (File path) -- TODO: setHeader???

setBodyEventSource :: Parser m => Event.EventSource -> m ()
setBodyEventSource source = setBody (EventSource source)

setPlaintext :: Parser m => Text.Text -> m ()
setPlaintext text = do
  setHeader ("Content-Type", "text/plain")
  setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

setHTML :: Parser m => LBS.ByteString -> m ()
setHTML html = do
  setHeader ("Content-Type", "text/html")
  setBody (Raw html)

setJSON :: (Aeson.ToJSON a, Parser m) => a -> m ()
setJSON value = do
  setHeader ("Content-Type", "application/json")
  setBodyRaw (Aeson.encode value)

addHeader :: Parser m => Header -> m ()
addHeader header = do
  headers <- gets headers
  modify (\response -> response {headers = header : headers})

addSetCookie :: Parser m => (BS.ByteString, BS.ByteString) -> m ()
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

write :: (Writeable a, Parser m) => a -> m ()
write value = do
  body <- gets body
  setBodyRaw $ case body of
    Raw raw -> raw <> toLBS value <> "\n"
    File _ -> toLBS value
    EventSource _ -> toLBS value

overwrite :: Parser m => LBS.ByteString -> m ()
overwrite = setBodyRaw

redirect :: Parser m => Status -> Text.Text -> m ()
redirect status url =
  let headers = [("Location", Text.encodeUtf8 url)]
      body = Raw ""
   in put Response {..}
