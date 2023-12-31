{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Path where

import Control.Applicative
import Control.Applicative.Combinators.NonEmpty qualified as Combinators
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.TypeLits qualified as TypeLits
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

data Parser a where
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: a -> Parser a
  Apply :: Parser (a -> b) -> Parser a -> Parser b
  Empty :: Parser a
  Or :: Parser a -> Parser a -> Parser a
  Match :: forall a. (Typeable.Typeable a, Web.ToHttpApiData a) => a -> Parser ()
  Param :: forall a. (Typeable.Typeable a, Web.FromHttpApiData a) => Parser a
  Splat :: forall a. (Typeable.Typeable a, Web.FromHttpApiData a) => Parser (NonEmpty.NonEmpty a)

-- Regex :: forall a. (Typeable.Typeable a, Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> Parser a

instance Functor Parser where
  fmap = FMap

instance Applicative Parser where
  pure = Pure
  (<*>) = Apply

instance Alternative Parser where
  empty = Empty
  (<|>) = Or

data LIT (s :: Exts.Symbol) where
  LIT :: (TypeLits.KnownSymbol s) => LIT s

data MATCH (a :: Type) where
  MATCH :: (Web.ToHttpApiData a) => a -> MATCH a

data PARAM (a :: Type) where
  PARAM :: (Web.FromHttpApiData a) => PARAM a

data SPLAT (a :: Type) where
  SPLAT :: (Web.FromHttpApiData a) => SPLAT (NonEmpty.NonEmpty a)

{-
data Nest (subRoute :: *) where
  Nest :: forall subRoute. (Path subRoute, Typeable.Typeable subRoute) => subRoute -> Nest subRoute
-}

class Path a where
  parser :: Parser a
  default parser :: (Generics.Generic a, GenericParser (Generics.Rep a)) => Parser a
  parser = fmap Generics.to genericParser
  printer :: a -> [Text.Text]
  default printer :: (Generics.Generic a, GenericPrinter (Generics.Rep a)) => a -> [Text.Text]
  printer = genericPrinter . Generics.from

instance Path Int where
  parser = param
  printer = pure . Web.toUrlPiece

instance Path Float where
  parser = param
  printer = pure . Web.toUrlPiece

instance Path Text.Text where
  parser = param
  printer = pure . Web.toUrlPiece

-- instance (Web.FromHttpApiData a, Web.ToHttpApiData a) => Path a where
--   parser = param @a
--   printer = pure . Web.toUrlPiece

class GenericParser f where
  genericParser :: Parser (f a)

instance GenericParser Generics.V1 where
  genericParser = empty

instance GenericParser Generics.U1 where
  genericParser = pure Generics.U1

instance (GenericParser f, GenericParser g) => GenericParser (f Generics.:+: g) where
  genericParser =
    fmap Generics.L1 genericParser <|> fmap Generics.R1 genericParser

instance (GenericParser f, GenericParser g) => GenericParser (f Generics.:*: g) where
  genericParser = liftA2 (Generics.:*:) genericParser genericParser

-- A data type
instance (GenericParser p) => GenericParser (Generics.M1 Generics.D f p) where
  genericParser = fmap Generics.M1 genericParser

-- Constructor with no arguments
instance {-# OVERLAPPING #-} (Generics.Constructor f) => GenericParser (Generics.M1 Generics.C f Generics.U1) where
  genericParser =
    let m :: Generics.M1 i f p a
        m = undefined
     in do
          lit $ Text.toLower $ Text.pack $ Generics.conName m
          x <- fmap Generics.M1 genericParser
          pure x

-- Constructor with one or more arguments
instance (Generics.Constructor f, GenericParser p) => GenericParser (Generics.M1 Generics.C f p) where
  genericParser =
    let m :: Generics.M1 i f p a
        m = undefined
     in fmap Generics.M1 genericParser

{-
-- A SubRoute
instance (Generics.Selector f, Path a, Typeable.Typeable a) => GenericParser (Generics.M1 Generics.S f (Generics.K1 i (Nest a))) where
  genericParser =
    let m :: Generics.M1 i f p a
        m = undefined
     in fmap (Generics.M1 . Generics.K1 . Nest) (parser @a)
-}

-- A Parameter/SubRoute
instance {-# OVERLAPPABLE #-} (Generics.Selector f, Path a, Typeable.Typeable a) => GenericParser (Generics.M1 Generics.S f (Generics.K1 i a)) where
  genericParser =
    let m :: Generics.M1 i f p a
        m = undefined
     in do
          p <- parser @a
          pure (Generics.M1 $ Generics.K1 p)

-- A Splat
instance (Generics.Selector f, Path a, Typeable.Typeable a) => GenericParser (Generics.M1 Generics.S f (Generics.K1 i (NonEmpty.NonEmpty a))) where
  genericParser =
    let m :: Generics.M1 i f p a
        m = undefined
     in do
          neList <- Combinators.some $ parser @a
          pure (Generics.M1 $ Generics.K1 neList)

-- A literal field
instance (Generics.Selector f, TypeLits.KnownSymbol s) => GenericParser (Generics.M1 Generics.S f (Generics.K1 i (LIT s))) where
  genericParser =
    let m :: Generics.M1 i f p a
        m = undefined
     in do
          lit $ Text.pack $ TypeLits.symbolVal @s Typeable.Proxy
          pure (Generics.M1 $ Generics.K1 LIT)

class GenericPrinter f where
  genericPrinter :: f a -> [Text.Text]

instance GenericPrinter Generics.V1 where
  genericPrinter _ = []

-- instance GenericParser Generics.U1 where
--   genericParser = pure Generics.U1

instance (GenericPrinter f, GenericPrinter g) => GenericPrinter (f Generics.:+: g) where
  genericPrinter (Generics.L1 l) = genericPrinter l
  genericPrinter (Generics.R1 r) = genericPrinter r

instance (GenericPrinter f, GenericPrinter g) => GenericPrinter (f Generics.:*: g) where
  genericPrinter (l Generics.:*: r) = genericPrinter l <> genericPrinter r

-- A data type
instance (GenericPrinter p) => GenericPrinter (Generics.M1 Generics.D f p) where
  genericPrinter (Generics.M1 x) = genericPrinter x

-- Constructor with one or more arguments
instance (Generics.Constructor f, GenericPrinter p) => GenericPrinter (Generics.M1 Generics.C f p) where
  genericPrinter (Generics.M1 x) =
    let m :: Generics.M1 i f p a
        m = undefined
     in genericPrinter x

-- Constructor with no arguments
instance {-# OVERLAPPING #-} (Generics.Constructor f) => GenericPrinter (Generics.M1 Generics.C f Generics.U1) where
  genericPrinter _ =
    let m :: Generics.M1 i f p a
        m = undefined
     in [Text.toLower $ Text.pack $ Generics.conName m]

-- A Parameter/Sub Path
instance {-# OVERLAPPABLE #-} (Generics.Selector f, Path a, Typeable.Typeable a) => GenericPrinter (Generics.M1 Generics.S f (Generics.K1 i a)) where
  genericPrinter (Generics.M1 (Generics.K1 x)) =
    let m :: Generics.M1 i f p a
        m = undefined
     in printer x

-- A Splat
instance (Generics.Selector f, Path a, Typeable.Typeable a) => GenericPrinter (Generics.M1 Generics.S f (Generics.K1 i (NonEmpty.NonEmpty a))) where
  genericPrinter (Generics.M1 (Generics.K1 x)) =
    let m :: Generics.M1 i f p a
        m = undefined
     in concat $ map printer $ NonEmpty.toList x

-- A literal field
instance (Generics.Selector f, TypeLits.KnownSymbol s) => GenericPrinter (Generics.M1 Generics.S f (Generics.K1 i (LIT s))) where
  genericPrinter (Generics.M1 (Generics.K1 _)) =
    let m :: Generics.M1 i f p a
        m = undefined
     in [Text.pack $ TypeLits.symbolVal @s Typeable.Proxy]

match :: forall a. (Typeable.Typeable a, Web.ToHttpApiData a) => a -> Parser ()
match = Match

lit :: Text.Text -> Parser ()
lit = Match @Text.Text

param :: (Typeable.Typeable a, Web.FromHttpApiData a) => Parser a
param = Param

splat :: (Typeable.Typeable a, Web.FromHttpApiData a) => Parser (NonEmpty.NonEmpty a)
splat = Splat

-- regex :: forall a. (Typeable.Typeable a, Regex.RegexContext Regex.Regex Text.Text a) => Text.Text -> Parser a
-- regex = Regex

-- allRoutes :: Parser a -> [Text.Text]
-- allRoutes (FMap _ dsl) = allRoutes dsl
-- allRoutes (Pure x) = ""
-- allRoutes (Apply aF aX) = allRoutes aF <> allRoutes aX
-- allRoutes (Match t) = "/" <> Web.toUrlPiece t
-- allRoutes (Param @p) = "/:" <> Text.pack (show . Typeable.typeRep $ Typeable.Proxy @p)
-- allRoutes (Regex @ty regex) = "/r(" <> regex <> ")"

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

data MyRoutes
  = Student
      { path :: LIT "student"
      , firstName :: Text.Text
      , lastName :: Text.Text
      }
  | StudentGrades
      { path' :: LIT "student"
      , firstName :: Text.Text
      , lastName :: Text.Text
      , rem :: LIT "grades"
      }
  | Hello
  | Baz
      { path'' :: LIT "baz"
      , subRoute :: SubRoute
      }
  deriving (Generics.Generic, Path)

data SubRoute = SubRoute {path :: LIT "student", foo :: Int, bar :: Float}
  deriving (Generics.Generic, Path)
