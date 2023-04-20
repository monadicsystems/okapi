module Okapi.Request where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP

type Request = (HTTP.StdMethod, [Text.Text], HTTP.Query, LBS.ByteString, HTTP.RequestHeaders)
