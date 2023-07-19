module Okapi.Spec.Request where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Parse as WAI

type Request = (HTTP.StdMethod, [Text.Text], HTTP.Query, IO LBS.ByteString {- Body.RequestBody -}, HTTP.RequestHeaders)

data RequestBody
  = RequestBodyRaw LBS.ByteString
  | RequestBodyMultipart ([WAI.Param], [WAI.File LBS.ByteString])
