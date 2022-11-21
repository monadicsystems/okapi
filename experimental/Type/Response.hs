{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Type.Response where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import Okapi.Event (EventSource)

type Header = (HTTP.HeaderName, BS.ByteString)

type Headers = [Header]

-- | Represents HTTP responses that can be returned by a parser.
data Response = Response
  { status :: Status,
    headers :: Headers,
    body :: Body
  }

type Status = Natural.Natural

-- | Represents the body of an HTTP response.
data Body
  = Raw LBS.ByteString
  | File FilePath
  | EventSource EventSource

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
