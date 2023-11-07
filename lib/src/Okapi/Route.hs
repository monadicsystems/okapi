{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Route where

import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Match :: forall a. (Web.ToHttpApiData a) => a -> Parser ()
  Param :: forall a. (Typeable.Typeable a, Web.FromHttpApiData a) => Parser a
  Regex :: forall a. (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

match :: forall a. (Web.ToHttpApiData a) => a -> Parser ()
match = Match

lit :: Text.Text -> Parser ()
lit = Match @Text.Text

param :: (Typeable.Typeable a, Web.FromHttpApiData a) => Parser a
param = Param

regex :: forall a. (Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> Parser a
regex = Regex

rep :: Parser a -> Text.Text
rep (FMap _ dsl) = rep dsl
rep (Pure x) = ""
rep (Apply aF aX) = rep aF <> rep aX
rep (Match t) = "/" <> Web.toUrlPiece t
rep (Param @p) = "/:" <> Text.pack (show . Typeable.typeRep $ Typeable.Proxy @p)
rep (Regex @ty regex) = "/r(" <> regex <> ")"

-- equals :: Parser a -> Parser b -> Bool
-- equals (FMap _ r) (FMap _ r') = equals r r'
-- equals (Pure _) (Pure _) = True
-- equals (Apply af ap) (Apply af' ap') = equals af af' && equals ap ap'
-- equals (Static t) (Static t') = t == t'
-- equals (Param @a) (Param @b) = case heqT @a @b of
--   Nothing -> False
--   Just HRefl -> True
-- equals _ _ = False

data Error = Error

parse :: Parser a -> [Text.Text] -> (Either Error a, [Text.Text])
parse = undefined