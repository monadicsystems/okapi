{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Path where

import qualified Control.Monad.Par as Par
import qualified Data.Text as Text
import qualified Debug.Trace as Debug
import qualified GHC.Generics as Generics
import qualified Web.HttpApiData as Web

data Error
  = ParseFail
  | NoMatch
  | EmptyPath
  | TooManyOperations
  | NotEnoughOperations
  deriving (Eq, Show, Generics.Generic, Par.NFData)

data Path a where
  FMap :: (a -> b) -> Path a -> Path b
  Pure :: a -> Path a
  Apply :: Path (a -> b) -> Path a -> Path b
  Static :: Text.Text -> Path ()
  Param :: Web.FromHttpApiData a => Path a

instance Functor Path where
  fmap :: (a -> b) -> Path a -> Path b
  fmap = FMap

instance Applicative Path where
  pure :: a -> Path a
  pure = Pure
  (<*>) :: Path (a -> b) -> Path a -> Path b
  (<*>) = Apply

eval ::
  Path a ->
  [Text.Text] ->
  (Either Error a, [Text.Text])
eval path state = case compare (countOps path) (length state) of
  LT -> (Left NotEnoughOperations, state)
  GT -> (Left TooManyOperations, state)
  EQ -> loop path state
  where
    loop ::
      Path a ->
      [Text.Text] ->
      (Either Error a, [Text.Text])
    loop path state = case path of
      FMap f opX ->
        case loop opX state of
          (Left e, state') -> (Left e, state')
          (Right x, state') -> (Right $ f x, state')
      Pure x -> (Right x, state)
      Apply opF opX -> case loop opF state of
        (Right f, state') -> case loop opX state' of
          (Right x, state'') -> (Right $ f x, state'')
          (Left e, state'') -> (Left e, state'')
        (Left e, state') -> (Left e, state')
      Static goal -> case state of
        [] -> (Left EmptyPath, state)
        (h : t) ->
          if h == goal
            then (Right (), t)
            else (Left NoMatch, t)
      Param -> case state of
        [] -> (Left EmptyPath, state)
        (h : t) -> case Web.parseUrlPieceMaybe h of
          Nothing -> (Left ParseFail, state)
          Just v -> (Right v, t)

countOps :: Path a -> Int
countOps path = case path of
  FMap _ opX -> countOps opX
  Pure _ -> 0
  Apply opF opX -> countOps opF + countOps opX
  Static _ -> 1
  Param -> 1

static :: Text.Text -> Path ()
static = Static

param :: Web.FromHttpApiData a => Path a
param = Param
