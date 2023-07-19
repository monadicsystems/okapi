{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Spec.Request.Security.Scheme where

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
import Okapi.Spec
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | ParamNotFound
  | APIKeyParseFailIn In
  | APIKeyNotFoundIn In BS.ByteString
  deriving (Eq, Show, Generics.Generic)

data In
  = Query
  | Headers
  | Cookie
  deriving (Eq, Show)

data Spec a where
  FMap :: (a -> b) -> Spec a -> Spec b
  Pure :: a -> Spec a
  Apply :: Spec (a -> b) -> Spec a -> Spec b
  APIKey :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => In -> BS.ByteString -> Spec a

instance Functor Spec where
  fmap = FMap

instance Applicative Spec where
  pure = Pure
  (<*>) = Apply

apiKey :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => In -> BS.ByteString -> Spec a
apiKey = APIKey

data State = State
  { query :: HTTP.Query,
    headers :: HTTP.RequestHeaders,
    cookies :: Web.Cookies
  }

eval ::
  Spec a ->
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
    Headers -> case findAPIKeyInHeader state.headers name of
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
  Nothing -> (Fail $ APIKeyNotFoundIn Headers name, headers)
  Just bs -> case Web.parseHeaderMaybe bs of
    Nothing -> (Fail $ APIKeyParseFailIn Headers, headers)
    Just value -> (Ok value, List.delete (CI.mk name, bs) headers)

findAPIKeyInQuery :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.Query -> BS.ByteString -> (Result Error a, HTTP.Query)
findAPIKeyInQuery query name = case lookup name query of
  Nothing -> (Fail $ APIKeyNotFoundIn Headers name, query)
  Just maybeBS -> case maybeBS of
    Nothing -> undefined
    Just bs -> case Web.parseHeaderMaybe bs of
      Nothing -> (Fail $ APIKeyParseFailIn Headers, query)
      Just value -> (Ok value, List.delete (name, Just bs) query)

class Interface a where
  parser :: Spec a

-- countOps :: Spec a -> Int
-- countOps path = case path of
--   FMap _ opX -> countOps opX
--   Pure _ -> 0
--   Apply opF opX -> countOps opF + countOps opX
--   Static _ -> 1
--   Param _ -> 1
