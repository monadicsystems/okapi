{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Effect.Response
  ( ok,
    notFound,
    redirect,
    -- RESPONSE BODY MODIFIERS
    plaintext,
    html,
    json,
    setBodyFile,
    setBodyEventSource,
    setBodyRaw,
    -- RESPONSE SETTERS
    setStatus,
    setHeaders,
    setHeader,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.Encoding as Text
import qualified GHC.Natural as Natural
import qualified Network.Server.Types as Server
import qualified Okapi.Event as Event
import Okapi.Types

-- BASE RESPONSES

ok :: Response
ok =
  let responseStatus = 200
      responseHeaders = []
      responseBody = ResponseBodyRaw "OK"
   in Response {..}

notFound :: Response
notFound =
  let responseStatus = 404
      responseHeaders = []
      responseBody = ResponseBodyRaw "Not Found"
   in Response {..}

redirect :: Status -> URL -> Response
redirect status (URL url) =
  let responseStatus = status
      responseHeaders = [("Location", encodeUtf8 url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}

-- RESPONSE BODY MODIFIERS

plaintext :: Text.Text -> Response -> Response
plaintext text response =
  response
    & setHeader ("Content-Type", "text/plain")
    & setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

html :: LBS.ByteString -> Response -> Response
html htmlRaw response =
  response
    & setBody (ResponseBodyRaw htmlRaw)
    & setHeader ("Content-Type", "text/html")

json :: forall a. Aeson.ToJSON a => a -> Response -> Response
json value response =
  response
    & setHeader ("Content-Type", "application/json")
    & setBodyRaw (Aeson.encode value)

setBodyFile :: FilePath -> Response -> Response
setBodyFile path = setBody (ResponseBodyFile path) -- TODO: setHeader???

setBodyEventSource :: EventSource -> Response -> Response
setBodyEventSource source response =
  response
    & setBody (ResponseBodyEventSource source)

-- RESPONSE SETTERS

setStatus :: Status -> Response -> Response
setStatus status response = response {responseStatus = status}

setHeaders :: Headers -> Response -> Response
setHeaders headers response = response {responseHeaders = headers}

-- TODO: setCookie

setHeader :: Header -> Response -> Response
setHeader header response@Response {..} =
  response {responseHeaders = update header responseHeaders}
  where
    update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    update pair [] = [pair]
    update pair@(key, value) (pair'@(key', value') : ps) =
      if key == key'
        then pair : ps
        else pair' : update pair ps

setBody :: ResponseBody -> Response -> Response
setBody body response = response {responseBody = body}

setBodyRaw :: LBS.ByteString -> Response -> Response
setBodyRaw bodyRaw = setBody (ResponseBodyRaw bodyRaw)
