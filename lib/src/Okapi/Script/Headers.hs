{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Script.Headers where

import Control.Monad.Par qualified as Par
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Okapi.Script
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | ParamNotFound
  | CookieHeaderNotFound
  | CookieNotFound
  | HeaderValueParseFail
  | CookieValueParseFail
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Script a where
  FMap :: (a -> b) -> Script a -> Script b
  Pure :: a -> Script a
  Apply :: Script (a -> b) -> Script a -> Script b
  Param :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.HeaderName -> Script a
  Cookie :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => BS.ByteString -> Script a
  Optional :: Script a -> Script (Maybe a)
  Option :: a -> Script a -> Script a

instance Functor Script where
  fmap = FMap

instance Applicative Script where
  pure = Pure
  (<*>) = Apply

param :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.HeaderName -> Script a
param = Param

cookie :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => BS.ByteString -> Script a
cookie = Cookie

optional :: Script a -> Script (Maybe a)
optional = Optional

option :: a -> Script a -> Script a
option = Option

eval ::
  Script a ->
  HTTP.RequestHeaders ->
  (Result Error a, HTTP.RequestHeaders)
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
  Param name -> case lookup name state of
    Nothing -> (Fail ParamNotFound, state)
    Just vBS -> case Web.parseHeaderMaybe vBS of
      Nothing -> (Fail HeaderValueParseFail, state)
      Just v -> (Ok v, List.delete (name, vBS) state)
  Cookie name -> case lookup "Cookie" state of
    Nothing -> (Fail CookieHeaderNotFound, state) -- TODO: Cookie not found
    Just cookiesBS -> case lookup name $ Web.parseCookies cookiesBS of
      Nothing -> (Fail CookieNotFound, state) -- TODO: Cookie parameter with given name not found
      Just valueBS -> case Web.parseHeaderMaybe valueBS of
        Nothing -> (Fail CookieValueParseFail, state)
        Just value ->
          ( Ok value,
            let headersWithoutCookie = List.delete ("Cookie", cookiesBS) state
             in ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete (name, valueBS) $ Web.parseCookies cookiesBS) : headersWithoutCookie {- List.delete (name, bs) headers -}
            -- TODO: Order of the cookie in the headers isn't preserved, but maybe this is fine??
          )
  Optional op' -> case op' of
    param@(Param _) -> case eval param state of
      (Ok result, state') -> (Ok $ Just result, state')
      (_, state') -> (Ok Nothing, state')
    cookie@(Cookie _) -> case eval cookie state of
      (Ok result, state') -> (Ok $ Just result, state')
      (_, state') -> (Ok Nothing, state')
    _ -> case eval op' state of
      (Ok result, state') -> (Ok $ Just result, state')
      (Fail err, state') -> (Fail err, state')
  Option def op' -> case op' of
    param@(Param _) -> case eval param state of
      (Ok result, state') -> (Ok result, state')
      (_, state') -> (Ok def, state')
    cookie@(Cookie _) -> case eval cookie state of
      (Ok result, state') -> (Ok result, state')
      (_, state') -> (Ok def, state')
    _ -> eval op' state
