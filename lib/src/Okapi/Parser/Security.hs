{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Parser.Security where

import Control.Applicative (Alternative ((<|>)), empty)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi qualified as OAPI
import Data.Set.NonEmpty (NESet)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Okapi.Parser
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Parser a where
  None :: Parser ()
  Lenient :: NonEmpty (Product a) -> Parser (Maybe a)
  Strict :: NonEmpty (Product a) -> Parser a

data Error
  = ParseFail
  | ParamNotFound
  | APIKeyParseFailIn In
  | APIKeyNotFoundIn In BS.ByteString
  deriving (Eq, Show, Generics.Generic)

data In
  = Query
  | Header
  | Cookie
  deriving (Eq, Show)

data Product a where
  FMap :: (a -> b) -> Product a -> Product b
  Pure :: a -> Product a
  Apply :: Product (a -> b) -> Product a -> Product b
  APIKey :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => In -> BS.ByteString -> Product a

instance Functor Product where
  fmap = FMap

instance Applicative Product where
  pure = Pure
  (<*>) = Apply

apiKey :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => In -> BS.ByteString -> Product a
apiKey = APIKey

data State = State
  { query :: HTTP.Query,
    headers :: HTTP.RequestHeaders,
    cookies :: Web.Cookies
  }

eval ::
  Product a ->
  State ->
  (Result Error a, State)
eval op state = case op of
  FMap f opX ->
    case eval opX state of
      (Fail e, state') -> (Fail e, state')
      (Ok x, state') -> (Ok $ f x, state')
  Pure x -> (Ok x, state)
  Apply opF opX -> case eval opF state of
    (Ok f, state') -> case eval opX state' of
      (Ok x, state'') -> (Ok $ f x, state'')
      (Fail e, state'') -> (Fail e, state'')
    (Fail e, state') -> (Fail e, state')
  APIKey inside name -> case inside of
    Query -> case findAPIKeyInQuery state.query name of
      (err@(Fail e), _) -> (err, state)
      (ok, newQuery) -> (ok, state {query = newQuery})
    Header -> case findAPIKeyInHeader state.headers name of
      (err@(Fail e), _) -> (err, state)
      (ok, newHeaders) -> (ok, state {headers = newHeaders})
    Cookie -> case findAPIKeyInCookie state.cookies name of
      (err@(Fail e), _) -> (err, state)
      (ok, newCookies) -> (ok, state {cookies = newCookies})

findAPIKeyInCookie :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => Web.Cookies -> BS.ByteString -> (Result Error a, Web.Cookies)
findAPIKeyInCookie cookies name = case lookup name cookies of
  Nothing -> (Fail $ APIKeyNotFoundIn Cookie name, cookies)
  Just bs -> case Web.parseHeaderMaybe bs of
    Nothing -> (Fail $ APIKeyParseFailIn Cookie, cookies)
    Just value -> (Ok value, List.delete (name, bs) cookies)

findAPIKeyInHeader :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.RequestHeaders -> BS.ByteString -> (Result Error a, HTTP.RequestHeaders)
findAPIKeyInHeader headers name = case lookup (CI.mk name) headers of
  Nothing -> (Fail $ APIKeyNotFoundIn Header name, headers)
  Just bs -> case Web.parseHeaderMaybe bs of
    Nothing -> (Fail $ APIKeyParseFailIn Header, headers)
    Just value -> (Ok value, List.delete (CI.mk name, bs) headers)

findAPIKeyInQuery :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.Query -> BS.ByteString -> (Result Error a, HTTP.Query)
findAPIKeyInQuery query name = case lookup name query of
  Nothing -> (Fail $ APIKeyNotFoundIn Header name, query)
  Just maybeBS -> case maybeBS of
    Nothing -> undefined
    Just bs -> case Web.parseHeaderMaybe bs of
      Nothing -> (Fail $ APIKeyParseFailIn Header, query)
      Just value -> (Ok value, List.delete (name, Just bs) query)

class Interface a where
  parser :: Parser a

-- countOps :: Parser a -> Int
-- countOps path = case path of
--   FMap _ opX -> countOps opX
--   Pure _ -> 0
--   Apply opF opX -> countOps opF + countOps opX
--   Static _ -> 1
--   Param _ -> 1
