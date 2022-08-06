{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Response
  ( _200,
    _204,
    _401,
    _403,
    _404,
    _500,
    redirect,
    -- RESPONSE BODY MODIFIERS
    plaintext,
    html,
    json,
    file,
    eventSource,
    -- RESPONSE SETTERS
    setStatus,
    setBody,
    setBodyRaw,
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
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Event as Event
import Okapi.Types

-- BASE RESPONSES

_200 :: Response
_200 =
  let responseStatus = 200
      responseHeaders = []
      responseBody = ResponseBodyRaw "OK"
   in Response {..}

_204 :: Response
_204 =
  let responseStatus = 204
      responseHeaders = []
      responseBody = ResponseBodyRaw "No Content"
   in Response {..}

_403 :: Response
_403 =
  let responseStatus = 403
      responseHeaders = []
      responseBody = ResponseBodyRaw "Forbidden"
   in Response {..}

_404 :: Response
_404 =
  let responseStatus = 404
      responseHeaders = []
      responseBody = ResponseBodyRaw "Not Found"
   in Response {..}

_401 :: Response
_401 =
  let responseStatus = 401
      responseHeaders = []
      responseBody = ResponseBodyRaw "Unauthorized"
   in Response {..}

_500 :: Response
_500 =
  let responseStatus = 500
      responseHeaders = []
      responseBody = ResponseBodyRaw "Internal Server Error"
   in Response {..}

redirect :: URL -> Response
redirect (URL url) =
  let responseStatus = 302
      responseHeaders = [("Location", encodeUtf8 url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}

-- RESPONSE BODY MODIFIERS

plaintext :: Text.Text -> Response -> Response
plaintext text response =
  response
    & setHeader ("Content-Type", "text/plain")
    & setBody (ResponseBodyRaw $ LBS.fromStrict . Text.encodeUtf8 $ text)

html :: LBS.ByteString -> Response -> Response
html htmlRaw response =
  response
    & setBody (ResponseBodyRaw htmlRaw)
    & setHeader ("Content-Type", "text/html")

json :: forall a. Aeson.ToJSON a => a -> Response -> Response
json value response =
  response
    & setHeader ("Content-Type", "application/json")
    & setBody (ResponseBodyRaw $ Aeson.encode value)

file :: FilePath -> Response -> Response
file path = setBody (ResponseBodyFile path) -- TODO: setHeader???

eventSource :: EventSource -> Response -> Response
eventSource source response =
  response
    & setBody (ResponseBodyEventSource source)

-- RESPONSE SETTERS

setStatus :: Natural.Natural -> Response -> Response
setStatus status response = response {responseStatus = status}

setHeaders :: Headers -> Response -> Response
setHeaders headers response = response {responseHeaders = headers}

-- TODO: setResponseCookie

setHeader :: HTTP.Header -> Response -> Response
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