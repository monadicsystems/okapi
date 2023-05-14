{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Parser.Query where

import Control.Monad.Par qualified as Par
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Okapi.Parser
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | FlagNotFound
  | ParamNotFound
  | ParamNoValue
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Param :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => BS.ByteString -> Parser a
  Flag :: BS.ByteString -> Parser ()
  Optional :: Parser a -> Parser (Maybe a)
  Option :: a -> Parser a -> Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

param :: (Web.FromHttpApiData a, OAPI.ToSchema a, Aeson.ToJSON a) => BS.ByteString -> Parser a
param = Param

flag :: BS.ByteString -> Parser ()
flag = Flag

optional :: (Web.FromHttpApiData a, OAPI.ToSchema a) => Parser a -> Parser (Maybe a)
optional = Optional

option :: (Web.FromHttpApiData a, OAPI.ToSchema a) => a -> Parser a -> Parser a
option = Option

eval ::
  Parser a ->
  HTTP.Query ->
  (Result Error a, HTTP.Query)
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
    Just maybeVBS -> case maybeVBS of
      Nothing -> (Fail ParamNoValue, state)
      Just vBS -> case Web.parseQueryParamMaybe $ Text.decodeUtf8 vBS of
        Nothing -> (Fail ParseFail, state)
        Just v -> (Ok v, List.delete (name, Just vBS) state)
  Flag name -> case lookup name state of
    Nothing -> (Fail FlagNotFound, state)
    Just found -> (Ok (), List.delete (name, found) state)
  Optional op' -> case op' of
    param@(Param _) -> case eval param state of
      (Ok result, state') -> (Ok $ Just result, state')
      (_, state') -> (Ok Nothing, state')
    flag@(Flag _) -> case eval flag state of
      (Ok result, state') -> (Ok $ Just result, state')
      (_, state') -> (Ok Nothing, state')
    _ -> case eval op' state of
      (Ok result, state') -> (Ok $ Just result, state')
      (Fail err, state') -> (Fail err, state')
  Option def op' -> case op' of
    param@(Param _) -> case eval param state of
      (Ok result, state') -> (Ok result, state')
      (_, state') -> (Ok def, state')
    flag@(Flag _) -> case eval flag state of
      (Ok result, state') -> (Ok result, state')
      (_, state') -> (Ok def, state')
    _ -> eval op' state

class Interface a where
  parser :: Parser a
