{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Response where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Natural as Natural
import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Event as Event
import Okapi.Internal.Types
import Okapi.Internal.Functions.Response

-- BASE RESPONSES

ok :: Response
ok =
  let responseStatus = 200
      responseHeaders = []
      responseBody = ResponseBodyRaw "OK"
   in Response {..}

noContent :: Response
noContent =
  let responseStatus = 204
      responseHeaders = []
      responseBody = ResponseBodyRaw "No Content"
   in Response {..}

forbidden :: Response
forbidden =
  let responseStatus = 403
      responseHeaders = []
      responseBody = ResponseBodyRaw "Forbidden"
   in Response {..}

notFound :: Response
notFound =
  let responseStatus = 404
      responseHeaders = []
      responseBody = ResponseBodyRaw "Not Found"
   in Response {..}

unauthorized :: Response
unauthorized =
  let responseStatus = 401
      responseHeaders = []
      responseBody = ResponseBodyRaw "Unauthorized"
   in Response {..}

internalServerError :: Response
internalServerError =
  let responseStatus = 500
      responseHeaders = []
      responseBody = ResponseBodyRaw "Internal Server Error"
   in Response {..}

-- TODO: Change type of URL?
redirectTo :: Char8.ByteString -> Response
redirectTo url =
  let responseStatus = 302
      responseHeaders = [("Location", url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}

-- RESPONSE BODY MODIFIERS

plaintext :: Text.Text -> Response -> Response
plaintext text response =
  response
    & setResponseHeader ("Content-Type", "text/plain")
    & setResponseBody (ResponseBodyRaw $ LazyByteString.fromStrict . Text.encodeUtf8 $ text)

html :: LazyByteString.ByteString -> Response -> Response
html htmlRaw response =
  response
    & setResponseBody (ResponseBodyRaw htmlRaw)
    & setResponseHeader ("Content-Type", "text/html")

json :: LazyByteString.ByteString -> Response -> Response
json bytes response =
  response
    & setResponseHeader ("Content-Type", "application/json")
    & setResponseBody (ResponseBodyRaw bytes)

file :: FilePath -> Response -> Response
file path = setResponseBody (ResponseBodyFile path) -- TODO: setHeader???

eventSource :: EventSource -> Response -> Response
eventSource source response =
  response
    & setResponseBody (ResponseBodyEventSource source)

aeson :: forall a. Aeson.ToJSON a => a -> Response -> Response
aeson = json . Aeson.encode

lucid :: forall a. Lucid.ToHtml a => a -> Response -> Response
lucid = html . Lucid.renderBS . Lucid.toHtml
