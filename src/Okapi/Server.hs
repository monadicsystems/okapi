module Okapi.Server where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text
import qualified Network.Wai as WAI
import qualified Okapi.Controller as Controller

data Info = Info
  { author :: Text.Text,
    name :: Text.Text
  }

data Server = Server
  { info :: Maybe Info,
    controllers :: NonEmpty Controller.Controller
  }

data Options = Options

genApplication ::
  Options ->
  Server ->
  WAI.Application
genApplication = undefined

genOpenAPISpec ::
  Server ->
  BS.ByteString
genOpenAPISpec = undefined

genJSClient ::
  Server ->
  BS.ByteString
genJSClient = undefined
