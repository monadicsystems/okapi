{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Headers where

import Data.Text
import Data.Typeable
import Web.HttpApiData qualified as Web

data Headers a where
  FMap :: (a -> b) -> Headers a -> Headers b
  Pure :: a -> Headers a
  Apply :: Headers (a -> b) -> Headers a -> Headers b
  Match :: Text -> Headers ()
  Param :: (Typeable a, Web.FromHttpApiData a) => Headers a

instance Functor Headers where
  fmap = FMap

instance Applicative Headers where
  pure = Pure
  (<*>) = Apply

param :: (Typeable a, Web.FromHttpApiData a) => Headers a
param = Param

match :: Text -> Headers ()
match = Match

rep :: Headers a -> Text
rep (FMap _ dsl) = rep dsl
rep (Pure x) = ""
rep (Apply aF aX) = rep aF <> rep aX
rep (Match t) = "/" <> t
rep (Param @p) = "/:" <> pack (show . typeRep $ Proxy @p)

-- equals :: Headers a -> Headers b -> Bool
-- equals (FMap _ r) (FMap _ r') = equals r r'
-- equals (Pure _) (Pure _) = True
-- equals (Apply af ap) (Apply af' ap') = equals af af' && equals ap ap'
-- equals (Static t) (Static t') = t == t'
-- equals (Param @a) (Param @b) = case heqT @a @b of
--   Nothing -> False
--   Just HRefl -> True
-- equals _ _ = False

data Error = Error

exec :: Headers a -> [Text] -> (Either Error a, [Text])
exec = undefined