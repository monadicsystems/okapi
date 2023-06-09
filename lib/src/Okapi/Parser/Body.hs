{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Parser.Body where

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
import Okapi.Parser
import Okapi.Parser.Body.Multipart qualified as Multipart
import Web.Cookie qualified as Web
import Web.FormUrlEncoded qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = JSONParseFail
  | URLEncodedParseFail Text.Text
  | URLEncodedInvalid Text.Text
  | NotMultipart
  | MultipartError Multipart.Error
  deriving (Eq, Show, Generics.Generic)

data Content a where
  JSON :: Aeson.FromJSON a => Content a
  URLEncoded :: Web.FromForm a => Content a
  Multipart :: Multipart.Parser a -> Content a

instance Functor Content

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  None :: Parser ()
  Optional :: NonEmpty (Content a) -> Parser (Maybe a)
  Required :: NonEmpty (Content a) -> Parser a

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

data RequestBody
  = RequestBodyRaw LBS.ByteString
  | RequestBodyMultipart ([WAI.Param], [WAI.File LBS.ByteString])

eval ::
  Parser a ->
  RequestBody ->
  (Result Error a, RequestBody)
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
  None -> (Ok (), state)
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
    RequestBodyMultipart parts -> case Multipart.eval parser parts of
      (Ok value, state') -> (Ok value, RequestBodyMultipart state')
      (Fail err, state') -> (Fail $ MultipartError err, RequestBodyMultipart state')

none :: Parser ()
none = None

json :: Aeson.FromJSON a => Parser a
json = JSON

urlEncoded :: Web.FromForm a => Parser a
urlEncoded = URLEncoded

multipart :: Multipart.Parser a -> Parser a
multipart = Multipart

class Interface a where
  parser :: NonEmpty (Parser a)

-- TODO: Add optional for body
{-
countOps :: Parser a -> Int
countOps path = case path of
  FMap _ opX -> countOps opX
  Pure _ -> 0
  Apply opF opX -> countOps opF + countOps opX
  JSON -> 1
  URLEncoded -> 1
  Multipart -> undefined
-}
