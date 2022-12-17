{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Response
  ( Parser (..),
    Response,
    Status,
    Headers,
    Header,
    ok,
    noContent,
    notFound,
    forbidden,
    internalServerError,
    Okapi.Response.status,
    Okapi.Response.headers,
    header,
    file,
    eventSource,
    plaintext,
    html,
    json,
    addHeader,
    setCookie,
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

status :: Parser m => Status -> m ()
status status = modify (\response -> response {status = status})

headers :: Parser m => Headers -> m ()
headers headers = modify (\response -> response {headers = headers})

-- TODO: If header with same name already exists in the headers list, add it to the value chain
header :: Parser m => Header -> m ()
header header = do
  headers' <- gets Okapi.Internal.Response.headers
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

file :: Parser m => FilePath -> m ()
file path = setBody (File path) -- TODO: header???

eventSource :: Parser m => Event.EventSource -> m ()
eventSource source = setBody (EventSource source)

plaintext :: Parser m => Text.Text -> m ()
plaintext text = do
  header ("Content-Type", "text/plain")
  setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

html :: Parser m => LBS.ByteString -> m ()
html html = do
  header ("Content-Type", "text/html")
  setBody (Raw html)

json :: (Aeson.ToJSON a, Parser m) => a -> m ()
json value = do
  header ("Content-Type", "application/json")
  setBodyRaw (Aeson.encode value)

addHeader :: Parser m => Header -> m ()
addHeader header = do
  headers <- gets Okapi.Internal.Response.headers
  modify (\response -> response {headers = header : headers})

setCookie :: Parser m => (BS.ByteString, BS.ByteString) -> m ()
setCookie (key, value) = do
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
