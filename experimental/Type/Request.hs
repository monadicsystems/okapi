{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Type.Request where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Network.Server.Types as Server
import qualified Network.Wai.Parse as WAI
import qualified Web.HttpApiData as Web

-- | Represents the Server request being parsed.
data Request = Request
  { method :: Method,
    path :: Path,
    query :: Query,
    body :: Body,
    headers :: Headers,
    vault :: Vault.Vault
  }

instance Eq Request where
  (==) :: Request -> Request -> Bool
  request1 == request2 =
    let sameMethod = method request1 == method request2
        samePath = path request1 == path request2
        sameQuery = query request1 == query request2
        sameBody = body request1 == body request2
        sameHeaders = headers request1 == headers request2
     in sameMethod && samePath && sameQuery && sameBody && sameHeaders

instance Show Request where
  show :: Request -> String
  show Request {..} =
    unlines
      [ "Method: " <> show method,
        "Path: " <> show path,
        "Query: " <> show query,
        "Body: " <> show body,
        "Headers: " <> show headers
      ]

type Method = Maybe BS.ByteString

type Path = [Text.Text]

type Query = [QueryItem]

type QueryItem = (Text.Text, QueryValue)

data QueryValue = QueryParam Text.Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

data Body
  = Raw LBS.ByteString
  | Multipart ([WAI.Param], [WAI.File LBS.ByteString])
  deriving (Eq, Show)

type Headers = [Header]

type Header = (HeaderName, BS.ByteString)

type HeaderName = Server.HeaderName

type Cookie = [Crumb]

type Crumb = (BS.ByteString, BS.ByteString)

-- $patterns

pattern GET :: Method
pattern GET = Just "GET"

pattern POST :: Method
pattern POST = Just "POST"

pattern PUT :: Method
pattern PUT = Just "PUT"

pattern PATCH :: Method
pattern PATCH = Just "PATCH"

pattern DELETE :: Method
pattern DELETE = Just "DELETE"

pattern TRACE :: Method
pattern TRACE = Just "TRACE"

pattern CONNECT :: Method
pattern CONNECT = Just "CONNECT"

pattern OPTIONS :: Method
pattern OPTIONS = Just "OPTIONS"

pattern HEAD :: Method
pattern HEAD = Just "HEAD"

pattern NULL :: Method
pattern NULL = Nothing

pattern PathParam :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> Text.Text
pattern PathParam param <-
  (Web.parseUrlPiece -> Right param)
  where
    PathParam param = Web.toUrlPiece param
{-
pattern IsQueryParam :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> QueryValue
pattern IsQueryParam param <-
  QueryParam (Web.parseUrlPiece -> Right param)
  where
    IsQueryParam param = QueryParam $ Web.toUrlPiece param

pattern HasQueryFlag :: Maybe QueryValue
pattern HasQueryFlag <- Just QueryFlag
-}