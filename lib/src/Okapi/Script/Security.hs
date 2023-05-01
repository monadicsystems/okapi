{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Script.Security where

import Control.Applicative (Alternative ((<|>)), empty)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.List qualified as List
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Okapi.Script
import Okapi.Script.Security.Secure qualified as Secure
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = SecureError Secure.Error
  deriving (Eq, Show, Generics.Generic)

data Script a where
  FMap :: (a -> b) -> Script a -> Script b
  None :: Script ()
  Secure :: Secure.Script a -> Script a

instance Functor Script where
  fmap :: (a -> b) -> Script a -> Script b
  fmap = FMap

eval ::
  Script a ->
  Secure.State ->
  (Result Error a, Secure.State)
eval op state = case op of
  FMap f script -> case eval script state of
    (Fail e, state') -> (Fail e, state')
    (Ok x, state') -> (Ok $ f x, state')
  None -> (Ok (), state)
  Secure script -> case Secure.eval script state of
    (Ok value, state') -> (Ok value, state')
    (Fail err, state') -> (Fail $ SecureError err, state')
