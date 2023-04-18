{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Headers where

import qualified Control.Monad.Par as Par
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.OpenApi as OAPI
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
  | HeaderValueParseFail
  | CookieValueParseFail
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Headers a where
  FMap :: (a -> b) -> Headers a -> Headers b
  Pure :: a -> Headers a
  Apply :: Headers (a -> b) -> Headers a -> Headers b
  Param :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.HeaderName -> Headers a
  Cookie :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => BS.ByteString -> Headers a
  Optional :: Headers a -> Headers (Maybe a)
  Option :: a -> Headers a -> Headers a

instance Functor Headers where
  fmap = FMap

instance Applicative Headers where
  pure = Pure
  (<*>) = Apply

param :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => HTTP.HeaderName -> Headers a
param = Param

cookie :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => BS.ByteString -> Headers a
cookie = Cookie

optional :: Headers a -> Headers (Maybe a)
optional = Optional

option :: a -> Headers a -> Headers a
option = Option

eval ::
  Headers a ->
  HTTP.RequestHeaders ->
  (Either Error a, HTTP.RequestHeaders)
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
  Param name -> case lookup name state of
    Nothing -> (Left ParamNotFound, state)
    Just vBS -> case Web.parseHeaderMaybe vBS of
      Nothing -> (Left HeaderValueParseFail, state)
      Just v -> (Right v, List.delete (name, vBS) state)
  Cookie name -> case lookup "Cookie" state of
    Nothing -> (Left CookieHeaderNotFound, state) -- TODO: Cookie not found
    Just cookiesBS -> case lookup name $ Web.parseCookies cookiesBS of
      Nothing -> (Left CookieNotFound, state) -- TODO: Cookie parameter with given name not found
      Just valueBS -> case Web.parseHeaderMaybe valueBS of
        Nothing -> (Left CookieValueParseFail, state)
        Just value ->
          ( Right value,
            let headersWithoutCookie = List.delete ("Cookie", cookiesBS) state
             in ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete (name, valueBS) $ Web.parseCookies cookiesBS) : headersWithoutCookie {- List.delete (name, bs) headers -}
            -- TODO: Order of the cookie in the headers isn't preserved, but maybe this is fine??
          )
  Optional op' -> case op' of
    param@(Param _) -> case eval param state of
      (Right result, state') -> (Right $ Just result, state')
      (_, state') -> (Right Nothing, state')
    cookie@(Cookie _) -> case eval cookie state of
      (Right result, state') -> (Right $ Just result, state')
      (_, state') -> (Right Nothing, state')
    _ -> case eval op' state of
      (Right result, state') -> (Right $ Just result, state')
      (Left err, state') -> (Left err, state')
  Option def op' -> case op' of
    param@(Param _) -> case eval param state of
      (Right result, state') -> (Right result, state')
      (_, state') -> (Right def, state')
    cookie@(Cookie _) -> case eval cookie state of
      (Right result, state') -> (Right result, state')
      (_, state') -> (Right def, state')
    _ -> eval op' state