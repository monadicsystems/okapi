{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Responder where

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
import qualified Okapi.Endpoint.ResponderHeaders as ResponderHeaders
import qualified Okapi.Endpoint.ResponderHeaders as ResponseHeaders
import qualified Web.Cookie as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (fmap, pure, return, (>>=))

data Error
  = ParseFail
  | ParamNotFound
  | CookieHeaderNotFound
  | CookieNotFound
  | ResponderHeadersError -- TODO: ResponderHeaders shouldn't be able to fail...
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Responder a where
  FMap :: (a -> b) -> Responder a -> Responder b
  Pure :: a -> Responder a
  Apply :: Responder (a -> b) -> Responder a -> Responder b
  JSON ::
    Aeson.ToJSON a =>
    HTTP.Status ->
    ResponderHeaders.ResponderHeaders h ->
    Responder
      ( (h %1 -> (ResponseHeaders.Response -> ResponseHeaders.Response)) ->
        a ->
        ResponseHeaders.Response
      )

fmap :: (a -> b) -> Responder a -> Responder b
fmap = FMap

pure :: a -> Responder a
pure = Pure

return = pure

(<*>) :: Responder (a -> b) -> Responder a -> Responder b
(<*>) = Apply

(>>=) = error "Was undefined"

eval ::
  Responder a ->
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
  JSON status responderHeaders -> case ResponderHeaders.eval responderHeaders () of
    (Right h, _) ->
      let f headerApplicator payload =
            ResponderHeaders.Response
              { ResponderHeaders.status = status,
                ResponderHeaders.body = Aeson.encode payload,
                ResponderHeaders.headers = []
              }
       in (Right f, state)
    (left, _) -> (Left ResponderHeadersError, state)

json ::
  Aeson.ToJSON a =>
  HTTP.Status ->
  ResponseHeaders.ResponderHeaders h ->
  Responder
    ( (h %1 -> (ResponseHeaders.Response -> ResponseHeaders.Response)) ->
      a ->
      ResponseHeaders.Response
    )
json = JSON