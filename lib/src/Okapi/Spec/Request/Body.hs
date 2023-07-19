{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Spec.Request.Body where

import Control.Monad.Par qualified as Par
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set.NonEmpty (NESet)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Parse (RequestBodyType (UrlEncoded))
import Network.Wai.Parse qualified as WAI
import Okapi.Spec
import Okapi.Spec.Request (RequestBody(..))
import Okapi.Spec.Request.Body.ContentType qualified as ContentType
import Web.Cookie qualified as Web
import Web.FormUrlEncoded qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = ContentTypeError ContentType.Error
  deriving (Eq, Show, Generics.Generic)

-- data ContentType a where
--   JSON :: Aeson.FromJSON a => ContentType a
--   URLEncoded :: Web.FromForm a => ContentType a
--   Multipart :: Form.Spec a -> ContentType a

-- instance Functor ContentType where
--   fmap :: (a -> b) -> ContentType a -> ContentType b
--   fmap _ _ = JSON   
--   fmap _ _ = URLEncoded
--   fmap f (Multipart form) = Multipart $ fmap f form

data Spec a where
  None :: Spec ()
  Optional :: NonEmpty (ContentType.Spec a) -> Spec (Maybe a)
  Required :: NonEmpty (ContentType.Spec a) -> Spec a

eval ::
  Spec a ->
  RequestBody ->
  (Result Error a, RequestBody)
eval spec state = case spec of
  None -> (Ok (), state) -- TODO: Should None only succeed if request body is empty??
  Optional (contentType :| tail) ->
    case ContentType.eval contentType state of
      (Ok value, state') -> (Ok $ Just value, state')
      (Fail error, state') -> case tail of
        [] -> (Ok Nothing, state')
        (h:t) -> eval (optional $ h :| t) state
  Required (contentType :| tail) ->
    case ContentType.eval contentType state of
      (Ok value, state') -> (Ok value, state')
      (Fail error, state') -> case tail of
        [] -> (Fail $ ContentTypeError error, state')
        (h:t) -> eval (required $ h :| t) state

none :: Spec ()
none = None

optional :: NonEmpty (ContentType.Spec a) -> Spec (Maybe a)
optional = Optional

required :: NonEmpty (ContentType.Spec a) -> Spec a
required = Required


class Interface a where
  parser :: NonEmpty (Spec a)
