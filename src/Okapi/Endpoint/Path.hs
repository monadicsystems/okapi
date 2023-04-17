{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Path where

import qualified Control.Monad.Par as Par
import qualified Data.Functor.Classes as Functor
import qualified Data.Text as Text
import qualified Debug.Trace as Debug
import qualified GHC.Generics as Generics
import qualified Web.HttpApiData as Web
import Prelude hiding (fmap, pure, return, (>>), (>>=))

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

fmap :: (a -> b) -> Path a -> Path b
fmap = FMap

pure :: a -> Path a
pure = Pure

return :: a -> Path a
return = pure

(<*>) :: Path (a -> b) -> Path a -> Path b
(<*>) = Apply

(>>=) :: Int ~ Char => f a -> (a -> f b) -> f b
(>>=) = error "Was undefined"

(>>) :: Int ~ Char => f a -> f b -> f b
(>>) = error "Was undefined"

-- instance Show a => Show (Path a) where
--   show (FMap _ p) = "FMap (" ++ "<function>" ++ ") " ++ show p
--   show (Pure a) = "Pure " <> show a
--   show (Apply _ pa) = "Apply (" ++ "<apply>" ++ ") (" ++ show pa ++ ")"
--   show (Static t) = "Static \"" ++ Text.unpack t ++ "\""
--   show Param = "Param"

-- TODO: Try using Rebindable Syntax and QualifiedDo to get rid of
-- the need for Functor and Applicative instances.

static :: Text.Text -> Path ()
static = Static

param :: (Show a, Web.FromHttpApiData a) => Path a
param = Param

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
            else (Left NoMatch, state)
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
