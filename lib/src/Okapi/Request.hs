module Okapi.Request where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Parser.Body as Body

type Request = (HTTP.StdMethod, [Text.Text], HTTP.Query, IO LBS.ByteString {- Body.RequestBody -}, HTTP.RequestHeaders)
