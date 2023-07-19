{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Spec.Request.Body.ContentType where

import Control.Monad.Par qualified as Par
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Set.NonEmpty (NESet)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Parse (RequestBodyType (UrlEncoded))
import Network.Wai.Parse qualified as WAI
import Okapi.Spec
import Okapi.Spec.Request (RequestBody(..))
import Okapi.Spec.Request.Body.Form qualified as Form
import Web.Cookie qualified as Web
import Web.FormUrlEncoded qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = JSONParseFail
  | URLEncodedParseFail Text.Text
  | URLEncodedInvalid Text.Text
  | NotMultipart
  | MultipartError Form.Error
  deriving (Eq, Show, Generics.Generic)

data Spec a where
  FMap :: (a -> b) -> Spec a -> Spec b
  JSON :: Aeson.FromJSON a => Spec a
  URLEncoded :: Web.FromForm a => Spec a
  Multipart :: Form.Spec a -> Spec a

instance Functor Spec where
  fmap = FMap

eval ::
  Spec a ->
  RequestBody ->
  (Result Error a, RequestBody)
eval op state = case op of
  FMap f opX ->
    case eval opX state of
      (Fail e, state') -> (Fail e, state')
      (Ok x, state') -> (Ok $ f x, state')
  JSON -> case state of
    RequestBodyRaw bs -> case Aeson.decode bs of
      Nothing -> (Fail JSONParseFail, state)
      Just value -> (Ok value, RequestBodyRaw mempty)
    RequestBodyMultipart _ -> (Fail JSONParseFail, state)
  URLEncoded -> case state of
    RequestBodyRaw bs -> case Web.urlDecodeForm bs of
      Left err -> (Fail $ URLEncodedInvalid err, state)
      Right form -> case Web.fromForm form of
        Left err -> (Fail $ URLEncodedParseFail err, state)
        Right value -> (Ok value, RequestBodyRaw mempty)
    RequestBodyMultipart (params, files) ->
      let bsParams = map (bimap Text.decodeUtf8 Text.decodeUtf8) params
          bs = Web.urlEncodeParams bsParams
       in case Web.urlDecodeForm bs of
            Left err -> (Fail $ URLEncodedInvalid err, state)
            Right form -> case Web.fromForm form of
              Left err -> (Fail $ URLEncodedParseFail err, state)
              Right value -> (Ok value, RequestBodyMultipart (mempty, files))
  Multipart parser -> case state of
    RequestBodyRaw _ -> (Fail NotMultipart, state)
    RequestBodyMultipart parts -> case Form.eval parser parts of
      (Ok value, state') -> (Ok value, RequestBodyMultipart state')
      (Fail err, state') -> (Fail $ MultipartError err, RequestBodyMultipart state')

json :: Aeson.FromJSON a => Spec a
json = JSON

urlEncoded :: Web.FromForm a => Spec a
urlEncoded = URLEncoded

multipart :: Form.Spec a -> Spec a
multipart = Multipart

class Interface a where
  parser :: Spec a
