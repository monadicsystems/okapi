{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Headers where

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

data Headers a where
  FMap :: (a -> b) -> Headers a -> Headers b
  Pure :: a -> Headers a
  Apply :: Headers (a -> b) -> Headers a -> Headers b
  Param :: Web.FromHttpApiData a => HTTP.HeaderName -> Headers a
  -- Flag :: BS.ByteString -> Headers ()
  Cookie :: BS.ByteString -> Headers BS.ByteString

instance Functor Headers where
  fmap = FMap

instance Applicative Headers where
  pure = Pure
  (<*>) = Apply

run ::
  Headers a ->
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
  Param name -> case lookup name state of
    Nothing -> (Left ParamNotFound, state)
    Just vBS -> case Web.parseHeaderMaybe vBS of
      Nothing -> (Left ParseFail, state)
      Just v -> (Right v, List.delete (name, vBS) state)
  Cookie name -> case lookup "Cookie" state of
    Nothing -> (Left CookieHeaderNotFound, state) -- TODO: Cookie not found
    Just cookiesBS -> case lookup name $ Web.parseCookies cookiesBS of
      Nothing -> (Left CookieNotFound, state) -- TODO: Cookie parameter with given name not found
      Just value ->
        ( Right value,
          let headersWithoutCookie = List.delete ("Cookie", cookiesBS) state
           in ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete (name, value) $ Web.parseCookies cookiesBS) : headersWithoutCookie {- List.delete (name, bs) headers -}
          -- TODO: Order of the cookie in the headers isn't preserved, but maybe this is fine??
        )
