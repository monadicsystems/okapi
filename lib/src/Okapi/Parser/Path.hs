{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Parser.Path where

import Control.Monad.Par qualified as Par
import Data.Functor.Classes qualified as Functor
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Debug.Trace qualified as Debug
import GHC.Generics qualified as Generics
import Generics.Kind
import Generics.Kind.TH
import Okapi.Parser
import Web.HttpApiData qualified as Web

data Error
  = ParseFail
  | NoMatch
  | EmptyPath
  | TooManyOperations
  | NotEnoughOperations
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Static :: Text.Text -> Parser ()
  Param :: (Typeable.Typeable a, Web.FromHttpApiData a, OAPI.ToSchema a) => Text.Text -> Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

-- TODO: Try using Rebindable Syntax and QualifiedDo to get rid of
-- the need for Functor and Applicative instances.

static :: Text.Text -> Parser ()
static = Static

param :: (Typeable.Typeable a, Web.FromHttpApiData a, OAPI.ToSchema a) => Text.Text -> Parser a
param = Param

eval ::
  Parser a ->
  [Text.Text] ->
  (Result Error a, [Text.Text])
eval path state = case compare (countOps path) (length state) of
  LT -> (Fail NotEnoughOperations, state)
  GT -> (Fail TooManyOperations, state)
  EQ -> loop path state
  where
    loop ::
      Parser a ->
      [Text.Text] ->
      (Result Error a, [Text.Text])
    loop path state = case path of
      FMap f opX ->
        case loop opX state of
          (Fail e, state') -> (Fail e, state')
          (Ok x, state') -> (Ok $ f x, state')
      Pure x -> (Ok x, state)
      Apply opF opX -> case loop opF state of
        (Ok f, state') -> case loop opX state' of
          (Ok x, state'') -> (Ok $ f x, state'')
          (Fail e, state'') -> (Fail e, state'')
        (Fail e, state') -> (Fail e, state')
      Static goal -> case state of
        [] -> (Fail EmptyPath, state)
        (h : t) ->
          if h == goal
            then (Ok (), t)
            else (Fail NoMatch, state)
      Param _ -> case state of
        [] -> (Fail EmptyPath, state)
        (h : t) -> case Web.parseUrlPieceMaybe h of
          Nothing -> (Fail ParseFail, state)
          Just v -> (Ok v, t)

countOps :: Parser a -> Int
countOps path = case path of
  FMap _ opX -> countOps opX
  Pure _ -> 0
  Apply opF opX -> countOps opF + countOps opX
  Static _ -> 1
  Param _ -> 1

renderPath :: Parser a -> Text.Text
renderPath path = case path of
  FMap f p -> renderPath p
  Pure _ -> mempty
  Apply pf px -> renderPath pf <> renderPath px
  Static t -> "/" <> t
  Param @p name -> "/{" <> name <> ":" <> Text.pack (show $ Typeable.typeOf @p undefined) <> "}"

class Interface a where
  parser :: Parser a