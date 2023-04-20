module Okapi.Response where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP

data Response = Response
  { status :: HTTP.Status,
    headers :: [ResponseHeader],
    body :: LBS.ByteString
  }

data ResponseHeader = ResponseHeader HTTP.HeaderName BS.ByteString
