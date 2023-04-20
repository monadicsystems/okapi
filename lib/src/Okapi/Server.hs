{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Server where

import qualified Control.Monad.Par as Par
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import Okapi.Executable
import qualified Okapi.Executable as Executable

data Info = Info
  { author :: Text.Text,
    name :: Text.Text
  }

type Compiler = Request -> Executable

data Server = Server
  { info :: Maybe Info,
    compilers :: [Compiler],
    defaultResponse :: WAI.Response
  }

data Options = Options

genApplication ::
  Options ->
  Server ->
  WAI.Application
genApplication _ server request respond = do
  let Right method = HTTP.parseMethod $ WAI.requestMethod request
      path = WAI.pathInfo request
      query = WAI.queryString request
      headers = WAI.requestHeaders request
  body <- WAI.strictRequestBody request
  let request = (method, path, query, body, headers)
      executables = map ($ request) $ compilers server
  case loop executables of
    Nothing -> respond server.defaultResponse
    Just action -> action >>= respond
  where
    loop :: [Executable] -> Maybe (IO WAI.Response)
    loop [] = Nothing
    loop (h : t) = case h of
      Run action -> Just action
      Null -> loop t

genOpenAPISpec ::
  Server ->
  BS.ByteString
genOpenAPISpec = undefined

genJSClient ::
  Server ->
  BS.ByteString
genJSClient = undefined
