{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Body where

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
  = JSONParseFail
  | FormParseFail
  | FormParamParseFail
  | FormParamNotFound
  | FileNotFound

data Body a where
  FMap :: (a -> b) -> Body a -> Body b
  Pure :: a -> Body a
  Apply :: Body (a -> b) -> Body a -> Body b
  FormParam :: Web.FromHttpApiData a => HTTP.HeaderName -> Body a

instance Functor Body where
  fmap = FMap

instance Applicative Body where
  pure = Pure
  (<*>) = Apply

run ::
  Body a ->
  HTTP.RequestHeaders ->
  (Either Error a, HTTP.RequestHeaders)
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
  FormParam name -> case lookup name state of
    Nothing -> (Left FormParamNotFound, state)
    Just vBS -> case Web.parseHeaderMaybe vBS of
      Nothing -> (Left FormParamParseFail, state)
      Just v -> (Right v, List.delete (name, vBS) state)
