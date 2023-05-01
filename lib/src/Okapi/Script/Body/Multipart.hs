{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Script.Body.Multipart where

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
import Network.Wai.Parse qualified as WAI
import Okapi.Script
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web

data Error
  = ParamParseFail
  | FileNotFound
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Script a where
  FMap :: (a -> b) -> Script a -> Script b
  Pure :: a -> Script a
  Apply :: Script (a -> b) -> Script a -> Script b
  Param :: Web.FromHttpApiData a => BS.ByteString -> Script a
  File :: BS.ByteString -> Script (WAI.FileInfo BS.ByteString)

instance Functor Script where
  fmap = FMap

instance Applicative Script where
  pure = Pure
  (<*>) = Apply

eval ::
  Script a ->
  ([WAI.Param], [WAI.File LBS.ByteString]) ->
  (Result Error a, ([WAI.Param], [WAI.File LBS.ByteString]))
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
  Param name -> undefined
  File name -> undefined

-- FormParam name -> case lookup name state of
--   Nothing -> (Fail FormParamNotFound, state)
--   Just vBS -> case Web.parseHeaderMaybe vBS of
--     Nothing -> (Fail FormParamParseFail, state)
--     Just v -> (Ok v, List.delete (name, vBS) state)

param :: Web.FromHttpApiData a => BS.ByteString -> Script a
param = Param

file :: BS.ByteString -> Script (WAI.FileInfo BS.ByteString)
file = File
