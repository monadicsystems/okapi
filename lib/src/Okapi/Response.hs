{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Response where

import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Identity qualified as Identity
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.Natural qualified as Natural
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Headers qualified as Headers
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Web.HttpApiData qualified as Web

class ToHeader a where
  toHeader :: a -> LBS.ByteString

type Elem :: Exts.Symbol -> [Exts.Symbol] -> Bool
type family Elem x ys where
  Elem x '[] = 'False
  Elem x (x ': ys) = 'True
  Elem x (y ': ys) = Elem x ys

-- type Remove :: Exts.Symbol -> [Exts.Symbol] -> [Exts.Symbol]
-- type family Remove (headerKey :: Exts.Symbol) (headerKeys :: [Exts.Symbol]) where
--   Remove _ '[] = '[]
--   Remove headerKey (h : t) = If (headerKey Equality.== h) t (h : Remove headerKey t)

type Remove :: Exts.Symbol -> [Exts.Symbol] -> [Exts.Symbol]
type family Remove x ys where
  Remove a '[] = '[]
  Remove a (a ': ys) = ys
  Remove a (y ': ys) = y ': (Remove a ys)

data Headers (headerKeys :: [Exts.Symbol]) where
  NoHeaders :: Headers '[]
  InsertHeader ::
    forall (headerKey :: Exts.Symbol) headerValue (headerKeys :: [Exts.Symbol]).
    (ToHeader headerValue) =>
    headerValue ->
    Headers headerKeys ->
    Headers (headerKey : headerKeys)

insertHeader ::
  forall (headerKey :: Exts.Symbol) headerValue (headerKeys :: [Exts.Symbol]).
  (ToHeader headerValue) =>
  headerValue ->
  Headers headerKeys ->
  Headers (headerKey : headerKeys)
insertHeader = InsertHeader

-- deleteHeader ::
--   forall (headerKey :: Exts.Symbol) (headerKeys :: [Exts.Symbol]).
--   (Elem headerKey headerKeys ~ True) =>
--   Headers headerKeys ->
--   Headers (Remove headerKey headerKeys)
-- deleteHeader NoHeaders = NoHeaders
-- deleteHeader (InsertHeader @k v rest) =
--   case Typeable.eqT @headerKey @k of
--     Nothing -> (deleteHeader @headerKey rest)
--     Just Typeable.Refl -> rest

data HeaderKey (k :: Exts.Symbol) = HeaderKey

-- instance Exts.KnownSymbol k => Show (Var k) where
--     show = Exts.symbolVal

-- | Membership test a type class (predicate)
class IsMember (headerKey :: Exts.Symbol) (headerKeys :: [Exts.Symbol]) where
  -- | Value-level lookup of elements from a map, via type class predicate
  lookupHeader :: HeaderKey headerKey -> Headers headerKeys -> LBS.ByteString

-- instance {-# OVERLAPS #-} IsMember v t ((v ':-> t) ': m) where
--   lookp _ (Ext _ x _) = x

instance {-# OVERLAPS #-} IsMember headerKey (headerKey ': rest) where
  lookupHeader _ (InsertHeader v _) = toHeader v

-- instance {-# OVERLAPPABLE #-} IsMember v t m => IsMember v t (x ': m) where
--   lookp v (Ext _ _ m) = lookp v m

instance {-# OVERLAPPABLE #-} (IsMember headerKey headerKeys) => IsMember headerKey (otherHeaderKey ': headerKeys) where
  lookupHeader k (InsertHeader _ tail) = lookupHeader k tail

{-
lookupHeader ::
  forall (headerKey :: Exts.Symbol) (headerKeys :: [Exts.Symbol]).
  (Elem headerKey headerKeys ~ True) =>
  Headers headerKeys ->
  LBS.ByteString
lookupHeader NoHeaders = undefined
lookupHeader (InsertHeader @k v rest) =
  case Typeable.eqT @headerKey @k of
    Nothing -> lookupHeader @headerKey rest
    Just Typeable.Refl -> toHeader v
-}

data Body
  = BodyStream Wai.StreamingBody
  | BodyBuilder Builder.Builder
  | BodyBytes LBS.ByteString
  | BodyFile FilePath Wai.FilePart

class ContentType a where
  contentTypeName :: LBS.ByteString
  contentTypeBody :: a -> Body

class (ContentType a) => ToContentType a b | b -> a where
  toContentType :: b -> a

data Response (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) where
  Response ::
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
    (ContentType contentType, ToContentType contentType resultType) =>
    Headers headerKeys ->
    resultType ->
    Response status headerKeys contentType resultType

data Builder a where
  FMap :: (a -> b) -> Builder a -> Builder b
  Pure :: a -> Builder a
  Apply :: Builder (a -> b) -> Builder a -> Builder b
  Has ::
    forall
      (status :: Natural.Natural)
      (headerKeys :: [Exts.Symbol])
      (contentType :: Type)
      (resultType :: Type).
    Builder
      ( Headers headerKeys ->
        resultType ->
        Response status headerKeys contentType resultType
      )

instance Functor Builder where
  fmap = FMap

instance Applicative Builder where
  pure = Pure
  (<*>) = Apply

has ::
  forall
    (status :: Natural.Natural)
    (headerKeys :: [Exts.Symbol])
    (contentType :: Type)
    (resultType :: Type).
  Builder
    ( Headers headerKeys ->
      resultType ->
      Response status headerKeys contentType resultType
    )
has = Has

-- equals :: Builder a -> Builder b -> Bool
-- equals (FMap _ r) (FMap _ r') = equals r r'
-- equals (Pure _) (Pure _) = True
-- equals (Apply af ap) (Apply af' ap') = equals af af' && equals ap ap'
-- equals (Has _) (Has _) = undefined
-- equals _ _ = False

class To a where
  builder :: Builder a
  build :: ()
