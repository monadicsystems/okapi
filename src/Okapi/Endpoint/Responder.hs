{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Responder where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Web.Cookie as Web
import qualified Web.HttpApiData as Web

data Error
  = ParseFail
  | ParamNotFound
  | CookieHeaderNotFound
  | CookieNotFound

data Response = Response
  { status :: HTTP.Status,
    headers :: HTTP.ResponseHeaders,
    body :: LBS.ByteString
  }

data Responder a where
  FMap :: (a -> b) -> Responder a -> Responder b
  Pure :: a -> Responder a
  Apply :: Responder (a -> b) -> Responder a -> Responder b
  JSON :: Aeson.ToJSON a => HTTP.Status -> Responder (a -> Response)

instance Functor Responder where
  fmap = FMap

instance Applicative Responder where
  pure = Pure
  (<*>) = Apply

run ::
  Responder a ->
  () ->
  (Either Error a, ())
run op state = case op of
  FMap f opX ->
    case run opX state of
      (Left e, state') -> (Left e, state')
      (Right x, state') -> (Right $ f x, state')
  Pure x -> (Right x, state)
  Apply opF opX -> case run opF state of
    (Right f, state') -> case run opX state' of
      (Right x, state'') -> (Right $ f x, state'')
      (Left e, state'') -> (Left e, state'')
    (Left e, state') -> (Left e, state')
  JSON status ->
    let responder = Response status [] . Aeson.encode
     in (Right responder, state)
