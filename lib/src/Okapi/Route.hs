{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Route where

import Data.Text
import Data.Typeable
import Web.HttpApiData qualified as Web

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Match :: Text -> Parser ()
  Param :: (Typeable a, Web.FromHttpApiData a) => Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

param :: (Typeable a, Web.FromHttpApiData a) => Parser a
param = Param

match :: Text -> Parser ()
match = Match

rep :: Parser a -> Text
rep (FMap _ dsl) = rep dsl
rep (Pure x) = ""
rep (Apply aF aX) = rep aF <> rep aX
rep (Match t) = "/" <> t
rep (Param @p) = "/:" <> pack (show . typeRep $ Proxy @p)

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

exec :: Parser a -> [Text] -> (Either Error a, [Text])
exec = undefined