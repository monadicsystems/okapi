{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Pattern where

import Data.Text qualified as Text
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai
import Web.HttpApiData qualified as Web
import Network.HTTP.Types qualified as HTTP

pattern Endpoint :: HTTP.Method -> [Text.Text] -> Wai.Request
pattern Endpoint requestMethod pathInfo <- Wai.Request { Wai.requestMethod, Wai.pathInfo }
  where
    Endpoint requestMethod pathInfo = Wai.defaultRequest { Wai.requestMethod = requestMethod, Wai.pathInfo = pathInfo }

pattern Param :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => a -> Text.Text
pattern Param x <- (Web.parseUrlPiece -> Right x)
  where
    Param x = Web.toUrlPiece x

pattern GET :: HTTP.Method
pattern GET = "GET"

pattern POST :: HTTP.Method
pattern POST = "POST"

pattern PUT :: HTTP.Method
pattern PUT = "PUT"

pattern HEAD :: HTTP.Method
pattern HEAD = "HEAD"

pattern DELETE :: HTTP.Method
pattern DELETE = "DELETE"

pattern TRACE :: HTTP.Method
pattern TRACE = "TRACE"

pattern CONNECT :: HTTP.Method
pattern CONNECT = "CONNECT"

pattern OPTIONS :: HTTP.Method
pattern OPTIONS = "OPTIONS"

pattern PATCH :: HTTP.Method
pattern PATCH = "PATCH"
