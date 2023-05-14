{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Okapi.Parser where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

data Result e a = Fail e | Ok a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifunctor Result where
  bimap f g (Fail x) = Fail (f x)
  bimap f g (Ok y) = Ok (g y)

instance Bifoldable Result where
  bifoldMap f g (Fail x) = f x
  bifoldMap f g (Ok y) = g y

instance Bitraversable Result where
  bitraverse f g (Fail x) = Fail <$> f x
  bitraverse f g (Ok y) = Ok <$> g y

instance Applicative (Result e) where
  pure = Ok
  Fail e <*> _ = Fail e
  _ <*> Fail e = Fail e
  Ok f <*> Ok x = Ok (f x)

instance Monad (Result e) where
  Fail e >>= _ = Fail e
  Ok x >>= f = f x

instance Monoid e => Alternative (Result e) where
  empty = Fail mempty

  Fail e1 <|> Fail e2 = Fail (e1 <> e2)
  Ok x <|> _ = Ok x
  _ <|> Ok y = Ok y
