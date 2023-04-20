{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Script.Responder where

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
import Okapi.Script
import Okapi.Script.ResponderHeaders (Response (..))
import Okapi.Script.ResponderHeaders qualified as ResponderHeaders
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | ParamNotFound
  | CookieHeaderNotFound
  | CookieNotFound
  | ResponderHeadersError -- TODO: ResponderHeaders shouldn't be able to fail...
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Script a where
  FMap :: (a -> b) -> Script a -> Script b
  Pure :: a -> Script a
  Apply :: Script (a -> b) -> Script a -> Script b
  JSON ::
    Aeson.ToJSON a =>
    HTTP.Status ->
    ResponderHeaders.Script h ->
    Script
      ( (h %1 -> (Response -> Response)) ->
        a ->
        Response
      )

instance Functor Script where
  fmap = FMap

instance Applicative Script where
  pure = Pure
  (<*>) = Apply

eval ::
  Script a ->
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
  JSON status responderHeaders -> case ResponderHeaders.eval responderHeaders () of
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
  ResponderHeaders.Script h ->
  Script
    ( (h %1 -> (Response -> Response)) ->
      a ->
      Response
    )
json = JSON