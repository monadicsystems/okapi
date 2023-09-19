{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Pattern where

import Data.Text qualified as Text
import Network.Wai qualified as Wai
import Web.HttpApiData qualified as Web
import Network.HTTP.Types qualified as HTTP
import Data.Tuple.Extra qualified as Extra
import Network.Socket qualified as Socket
import Data.ByteString qualified as BS
import Data.Vault.Lazy qualified as Vault

pattern Match
  :: HTTP.Method
  -> HTTP.HttpVersion
  -> HTTP.RequestHeaders
  -> Bool
  -> Socket.SockAddr
  -> [Text.Text]
  -> HTTP.Query
  -> IO BS.ByteString
  -> Vault.Vault
  -> Wai.RequestBodyLength
  -> Maybe BS.ByteString
  -> Maybe BS.ByteString
  -> Maybe BS.ByteString
  -> Maybe BS.ByteString
  -> Wai.Request
pattern Match
  { method
  , version
  , headers
  , isSecure
  , host
  , path
  , query
  , bodyStream
  , vault
  , bodyLength
  , headerHost
  , headerRange
  , headerReferer
  , headerUserAgent
  } <-
    ( match ->
        ( method
        , version
        , headers
        , isSecure
        , host
        , path
        , query
        , bodyStream
        , vault
        , bodyLength
        , headerHost
        , headerRange
        , headerReferer
        , headerUserAgent
        )
    )

match
  :: Wai.Request
  -> ( HTTP.Method
     , HTTP.HttpVersion
     , HTTP.RequestHeaders
     , Bool
     , Socket.SockAddr
     , [Text.Text]
     , HTTP.Query
     , IO BS.ByteString
     , Vault.Vault
     , Wai.RequestBodyLength
     , Maybe BS.ByteString
     , Maybe BS.ByteString
     , Maybe BS.ByteString
     , Maybe BS.ByteString
     )
match request =
  ( Wai.requestMethod request
  , Wai.httpVersion request
  , Wai.requestHeaders request
  , Wai.isSecure request
  , Wai.remoteHost request
  , Wai.pathInfo request
  , Wai.queryString request
  , Wai.getRequestBodyChunk request
  , Wai.vault request
  , Wai.requestBodyLength request
  , Wai.requestHeaderHost request
  , Wai.requestHeaderRange request
  , Wai.requestHeaderReferer request
  , Wai.requestHeaderUserAgent request
  )
{-
pattern Endpoint :: HTTP.Method -> [Text.Text] -> Wai.Request
pattern Endpoint method path <- Match { method, path }
  where
    Endpoint method path = Wai.defaultRequest { Wai.requestMethod = method, Wai.pathInfo = path }
-}
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
