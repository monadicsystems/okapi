{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Okapi.Tree (
    Tree (..),
    SymTree,
    (=.),
    cost,
    grow,
    eat,
) where

import Data.Kind (Type)
import Data.Profunctor
import Okapi.Leaf (ErrorOf, Parser, Printer, StateOf)

type Tree :: (Type -> Type) -> Type -> Type -> Type
data Tree t i o where
    FMap  :: (o -> o') -> Tree t i o -> Tree t i o'
    LMap  :: (i -> i') -> Tree t i' o -> Tree t i o
    Pure  :: o -> Tree t i o
    Apply :: Tree t i (o -> o') -> Tree t i o -> Tree t i o'
    Node  :: t a -> Tree t a a

type SymTree t a = Tree t a a

instance Functor (Tree t i) where
    fmap = FMap

instance Applicative (Tree t i) where
    pure = Pure
    (<*>) = Apply

instance Profunctor (Tree t) where
    rmap = FMap
    lmap = LMap

(=.) :: (Profunctor p) => (a -> b) -> p b c -> p a c
(=.) = lmap
infixr 5 =.

cost :: Tree t i o -> Int
cost = \case
    FMap _ c   -> cost c
    LMap _ c   -> cost c
    Pure _     -> 0
    Apply c c' -> cost c + cost c'
    Node _     -> 1

-- | Seeds grow into trees: parse raw wire format into a structured value.
grow ::
    forall t i o.
    (forall a. t a -> Parser t a) ->
    Tree t i o ->
    Parser t o
grow alg = go
  where
    go :: forall i' o'. Tree t i' o' -> Parser t o'
    go (Pure x) s = (Right x, s)
    go (FMap f c) s = case go c s of
        (Left e, s')  -> (Left e, s')
        (Right x, s') -> (Right (f x), s')
    go (LMap _ c) s = go c s
    go (Apply cf cx) s = case go cf s of
        (Left e, s1)  -> (Left e, s1)
        (Right f, s1) -> case go cx s1 of
            (Left e, s2)  -> (Left e, s2)
            (Right x, s2) -> (Right (f x), s2)
    go (Node t) s = alg t s

-- | Okapi eats trees and produces seeds: fold a structured value into raw wire format.
eat ::
    forall t i o.
    (Monoid (StateOf t)) =>
    (forall a. t a -> Printer t a) ->
    Tree t i o ->
    Printer t i
eat alg = go
  where
    go :: forall i' o'. Tree t i' o' -> Printer t i'
    go (Pure _) _      = mempty
    go (FMap _ c) i    = go c i
    go (LMap f c) i    = go c (f i)
    go (Apply cf cx) i = go cf i <> go cx i
    go (Node t) i      = alg t i
