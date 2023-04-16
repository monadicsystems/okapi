{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.ResponderHeaders where

import qualified Control.Monad.Par as Par
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Generics as Generics
import qualified Network.HTTP.Types as HTTP
import qualified Web.Cookie as Web
import qualified Web.HttpApiData as Web

data Error
  = ParseFail
  | ParamNotFound
  | CookieHeaderNotFound
  | CookieNotFound
  | ResponderHeadersError -- TODO: ResponderHeaders shouldn't be able to fail...
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Response = Response
  { status :: HTTP.Status,
    headers :: [ResponseHeader],
    body :: LBS.ByteString
  }

data ResponseHeader = ResponseHeader HTTP.HeaderName BS.ByteString

data ResponderHeaders a where
  FMap :: (a -> b) -> ResponderHeaders a -> ResponderHeaders b
  Pure :: a -> ResponderHeaders a
  Apply :: ResponderHeaders (a -> b) -> ResponderHeaders a -> ResponderHeaders b
  Has :: Web.ToHttpApiData a => HTTP.HeaderName -> ResponderHeaders (a -> (Response -> Response))

instance Functor ResponderHeaders where
  fmap :: (a -> b) -> ResponderHeaders a -> ResponderHeaders b
  fmap = FMap

instance Applicative ResponderHeaders where
  pure :: a -> ResponderHeaders a
  pure = Pure
  (<*>) :: ResponderHeaders (a -> b) -> ResponderHeaders a -> ResponderHeaders b
  (<*>) = Apply

eval ::
  ResponderHeaders a ->
  () ->
  (Either Error a, ())
eval op state = case op of
  FMap f opX ->
    case eval opX state of
      (Left e, state') -> (Left e, state')
      (Right x, state') -> (Right $ f x, state')
  Pure x -> (Right x, state)
  Apply opF opX -> case eval opF state of
    (Right f, state') -> case eval opX state' of
      (Right x, state'') -> (Right $ f x, state'')
      (Left e, state'') -> (Left e, state'')
    (Left e, state') -> (Left e, state')
  Has headerName ->
    let f value response = response {headers = headers response <> [ResponseHeader headerName $ Web.toHeader value]}
     in (Right f, state)

has ::
  Web.ToHttpApiData a =>
  HTTP.HeaderName ->
  ResponderHeaders
    (a -> Response -> Response)
has = Has