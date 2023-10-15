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

data Route a where
  FMap :: (a -> b) -> Route a -> Route b
  Pure :: a -> Route a
  Apply :: Route (a -> b) -> Route a -> Route b
  Match :: Text -> Route ()
  Param :: (Typeable a, Web.FromHttpApiData a) => Route a

instance Functor Route where
  fmap = FMap

instance Applicative Route where
  pure = Pure
  (<*>) = Apply

param :: (Typeable a, Web.FromHttpApiData a) => Route a
param = Param

match :: Text -> Route ()
match = Match

rep :: Route a -> Text
rep (FMap _ dsl) = rep dsl
rep (Pure x) = ""
rep (Apply aF aX) = rep aF <> rep aX
rep (Match t) = "/" <> t
rep (Param @p) = "/:" <> pack (show . typeRep $ Proxy @p)

-- equals :: Route a -> Route b -> Bool
-- equals (FMap _ r) (FMap _ r') = equals r r'
-- equals (Pure _) (Pure _) = True
-- equals (Apply af ap) (Apply af' ap') = equals af af' && equals ap ap'
-- equals (Static t) (Static t') = t == t'
-- equals (Param @a) (Param @b) = case heqT @a @b of
--   Nothing -> False
--   Just HRefl -> True
-- equals _ _ = False

data Error = Error

exec :: Route a -> [Text] -> (Either Error a, [Text])
exec = undefined