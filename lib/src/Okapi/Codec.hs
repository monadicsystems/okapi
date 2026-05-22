{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Codec (
    Codec (..),
    IsoCodec (..),
    Value (..),
    StateOf,
    ParseErrorOf,
    Parser,
    Printer,
    cost,
    parser,
    printer,
)
where

import Data.Kind (Type)
import Data.Profunctor

type Codec :: (Type -> Type) -> Type -> Type -> Type
data Codec t i o where
    FMap  :: (o -> o') -> Codec t i o -> Codec t i o'
    LMap  :: (i -> i') -> Codec t i' o -> Codec t i o
    Pure  :: o -> Codec t i o
    Apply :: Codec t i (o -> o') -> Codec t i o -> Codec t i o'
    Embed :: t a -> Codec t a a

newtype IsoCodec (t :: Type -> Type) a = IsoCodec {isoCodec :: Codec t a a}

newtype Value (t :: Type -> Type) a = Value {value :: a}

instance Functor (Codec t i) where
    fmap = FMap

instance Applicative (Codec t i) where
    pure = Pure
    (<*>) = Apply

instance Profunctor (Codec t) where
    rmap = FMap
    lmap = LMap

type family StateOf      (t :: Type -> Type) :: Type
type family ParseErrorOf (t :: Type -> Type) :: Type

type Parser  t a = StateOf t -> (Either (ParseErrorOf t) a, StateOf t)
type Printer t a = a -> StateOf t

cost :: Codec t i o -> Int
cost = \case
    FMap _ c   -> cost c
    LMap _ c   -> cost c
    Pure _     -> 0
    Apply c c' -> cost c + cost c'
    Embed _    -> 1

parser ::
    forall t i o.
    (forall a. t a -> Parser t a) ->
    Codec t i o ->
    Parser t o
parser alg = go
  where
    go :: forall i' o'. Codec t i' o' -> Parser t o'
    go (Pure x)      s = (Right x, s)
    go (FMap f c)    s = case go c s of
        (Left e,  s') -> (Left e,      s')
        (Right x, s') -> (Right (f x), s')
    go (LMap _ c)    s = go c s
    go (Apply cf cx) s = case go cf s of
        (Left e,  s1) -> (Left e, s1)
        (Right f, s1) -> case go cx s1 of
            (Left e,  s2) -> (Left e,      s2)
            (Right x, s2) -> (Right (f x), s2)
    go (Embed t)     s = alg t s

printer ::
    forall t i o.
    Monoid (StateOf t) =>
    (forall a. t a -> Printer t a) ->
    Codec t i o ->
    Printer t i
printer alg = go
  where
    go :: forall i' o'. Codec t i' o' -> Printer t i'
    go (Pure _)      _ = mempty
    go (FMap _ c)    i = go c i
    go (LMap f c)    i = go c (f i)
    go (Apply cf cx) i = go cf i <> go cx i
    go (Embed t)     i = alg t i
