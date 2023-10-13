{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Okapi.Route where

import Data.Text
import Data.Typeable
import Web.HttpApiData qualified as Web
-- import Prelude hiding (fmap, (<$>), pure, return, (<*>))

-- import Distribution.Simple.Setup (testOptions')

-- TODO!!!!! DO NOT NEED constraints on Applicative methods, so make it Applicative!!!!

data Route a where
  FMap :: (a -> b) -> Route a -> Route b
  Pure :: a -> Route a
  Apply :: Route (a -> b) -> Route a -> Route b
  Static :: Text -> Route ()
  Param :: (Typeable a, Web.FromHttpApiData a) => Route a
  -- Assert :: (a -> Bool) -> Route a ???? Do we need this.

-- fmap :: (a -> b) -> Route a -> Route b
-- fmap = FMap

-- pure :: a -> Route a
-- pure = Pure

-- return :: a -> Route a
-- return = Pure

-- (<$>) :: (a -> b) -> Route a -> Route b
-- (<$>) = FMap

-- (<*>) :: Route (a -> b) -> Route a -> Route b
-- (<*>) = Apply

-- (>>=) = undefined

-- (>>) = undefined

param :: (Typeable a, Web.FromHttpApiData a) => Route a
param = Param

static :: Text -> Route ()
static = Static

instance Functor Route where
    fmap = FMap

instance Applicative Route where
    pure = Pure
    (<*>) = Apply

heq :: Route a -> Route b -> Bool
heq (FMap _ r) (FMap _ r') = heq r r'
heq (Pure _) (Pure _) = True
heq (Apply af ap) (Apply af' ap') = heq af af' && heq ap ap'
heq (Static t) (Static t') = t == t'
heq (Param @a) (Param @b) = case eqT @a @b of
  Nothing -> False
  Just Refl -> True
heq _ _ = False

showRoute :: forall a. Route a -> Text
showRoute (FMap _ dsl) = showRoute dsl
showRoute (Pure x) = ""
showRoute (Apply aF aX) = showRoute aF <> showRoute aX
showRoute (Static t) = "/" <> t
showRoute (Param @p) = "/:" <> pack (convert @p)

-- instance (Show a, Typeable a) => Show (Route a) where
--     show (FMap f dsl) = "FMap <function> (" <> show dsl <> ")"
--     show (Pure x) = "Pure " <> show x
--     show (Apply aF aX) = "Apply <apFunction> (" <> show aX <> ")"
--     show (Static t) = "Static " <> unpack t
--     show (Param @p) = "Param " <> convert @p

convert :: forall a. Typeable a => String
convert = show . typeRep $ Proxy @a

{-
(===) :: (Eq a, Typeable a, Eq b, Typeable b) => Route a -> Route b -> Bool
FMap f dsl === FMap f' dsl' = dsl === dsl'
Pure x === Pure x' = Just x == cast x'
Apply fR xR === Apply fR' xR' = (===) xR xR'
Static t === Static t' = t == t'
Param @a === Param @a' = case eqT @a @a' of
    Nothing -> False
    Just Refl -> True
Assert _ === _ = False
_ === Assert _ = False
_ === _ = False
-}