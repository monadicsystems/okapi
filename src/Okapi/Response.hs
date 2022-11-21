{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Response
  ( MonadResponse (..),
    State (..),
    gets,
    modify,
    look,
    Response (..),
    Status,
    Headers,
    Header,
    Body (..),
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
    Writeable (..),
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
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Error as Error
import qualified Okapi.Event as Event
import qualified Web.Cookie as Web

type MonadResponse m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

class Monad m => State m where
  get :: m Response
  put :: Response -> m ()

gets :: State m => (Response -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Response -> Response) -> m ()
modify modifier = do
  response <- get
  put $ modifier response

look :: State m => m a -> m a
look action = do
  response <- get
  result <- action
  put response
  pure result

-- | Represents HTTP responses that can be returned by a parser.
data Response = Response
  { status :: Status,
    headers :: Headers,
    body :: Body
  }

type Status = Natural.Natural

type Headers = [Header]

type Header = (HTTP.HeaderName, BS.ByteString)

-- | Represents the body of an HTTP response.
data Body
  = Raw LBS.ByteString
  | File FilePath
  | EventSource Event.EventSource

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

-- response :: MonadResponse m => m Response
-- response = get

-- RESPONSE START --

setStatus :: MonadResponse m => Status -> m ()
setStatus status = modify (\response -> response {status = status})

setHeaders :: MonadResponse m => Headers -> m ()
setHeaders headers = modify (\response -> response {headers = headers})

setHeader :: MonadResponse m => Header -> m ()
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

setBody :: MonadResponse m => Body -> m ()
setBody body = modify (\response -> response {body = body})

setBodyRaw :: MonadResponse m => LBS.ByteString -> m ()
setBodyRaw lbs = setBody (Raw lbs)

setBodyFile :: MonadResponse m => FilePath -> m ()
setBodyFile path = setBody (File path) -- TODO: setHeader???

setBodyEventSource :: MonadResponse m => Event.EventSource -> m ()
setBodyEventSource source = setBody (EventSource source)

setPlaintext :: MonadResponse m => Text.Text -> m ()
setPlaintext text = do
  setHeader ("Content-Type", "text/plain")
  setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

setHTML :: MonadResponse m => LBS.ByteString -> m ()
setHTML html = do
  setHeader ("Content-Type", "text/html")
  setBody (Raw html)

setJSON :: (Aeson.ToJSON a, MonadResponse m) => a -> m ()
setJSON value = do
  setHeader ("Content-Type", "application/json")
  setBodyRaw (Aeson.encode value)

addHeader :: MonadResponse m => Header -> m ()
addHeader header = do
  headers <- gets headers
  modify (\response -> response {headers = header : headers})

addSetCookie :: MonadResponse m => (BS.ByteString, BS.ByteString) -> m ()
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

class Writeable a where
  toLBS :: a -> LBS.ByteString
  default toLBS :: Show a => a -> LBS.ByteString
  toLBS = LBS.fromStrict . Text.encodeUtf8 . Text.pack . Prelude.takeWhile ('"' /=) . Prelude.dropWhile ('"' ==) . show

instance Writeable Text.Text

instance Writeable LBS.ByteString where
  toLBS :: LBS.ByteString -> LBS.ByteString
  toLBS = id

instance Writeable Int

write :: (Writeable a, MonadResponse m) => a -> m ()
write value = do
  body <- gets body
  setBodyRaw $ case body of
    Raw raw -> raw <> toLBS value <> "\n"
    File _ -> toLBS value
    EventSource _ -> toLBS value

overwrite :: MonadResponse m => LBS.ByteString -> m ()
overwrite = setBodyRaw

redirect :: MonadResponse m => Status -> Text.Text -> m ()
redirect status url =
  let headers = [("Location", Text.encodeUtf8 url)]
      body = Raw ""
   in put Response {..}
