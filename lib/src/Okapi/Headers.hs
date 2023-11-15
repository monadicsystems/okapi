{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Headers where

import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Web.HttpApiData qualified as Web

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Match :: forall a. (Web.ToHttpApiData a) => a -> Parser ()
  Param :: forall a. (Typeable.Typeable a, Web.FromHttpApiData a) => Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

class From a where
  parser :: Parser a
  parse :: ()
