{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Spec.Response where

import Control.Monad.Par qualified as Par
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Okapi.Spec
import Okapi.Spec.Response.Headers (Response (..))
import Okapi.Spec.Response.Headers qualified as Headers
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | ParamNotFound
  | CookieHeaderNotFound
  | CookieNotFound
  | ResponderHeadersError -- TODO: Headers shouldn't be able to fail...
  deriving (Eq, Show, Generics.Generic, Par.NFData)

-- TODO: Use Non-empty list to represent spec instead of a do-block?
data Spec a where
  FMap :: (a -> b) -> Spec a -> Spec b
  Pure :: a -> Spec a
  Apply :: Spec (a -> b) -> Spec a -> Spec b
  JSON ::
    Aeson.ToJSON a =>
    HTTP.Status ->
    Headers.Spec h ->
    Spec
      ( (h %1 -> (Response -> Response)) ->
        a ->
        Response
      )

instance Functor Spec where
  fmap = FMap

instance Applicative Spec where
  pure = Pure
  (<*>) = Apply

eval ::
  Spec a ->
  () ->
  (Result Error a, ())
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
  JSON status responderHeaders -> case Headers.eval responderHeaders () of
    (Ok h, _) ->
      let f headerApplicator payload =
            Response
              { status = status,
                body = Aeson.encode payload,
                headers = []
              }
       in (Ok f, state)
    (left, _) -> (Fail ResponderHeadersError, state)

json ::
  Aeson.ToJSON a =>
  HTTP.Status ->
  Headers.Spec h ->
  Spec
    ( (h %1 -> (Response -> Response)) ->
      a ->
      Response
    )
json = JSON

class Interface a where
  parser :: Spec a