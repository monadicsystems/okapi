{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Server.Request where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Parse as WAI

-- | Represents the HTTP request being parsed.
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

type HeaderName = HTTP.HeaderName

type Cookie = [Crumb]

type Crumb = (BS.ByteString, BS.ByteString)

class Monad m => StateM m where
  get :: m Request
  put :: Request -> m ()

gets :: StateM m => (Request -> a) -> m a
gets projection = projection <$> get

modify :: StateM m => (Request -> Request) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: StateM m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result
