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

import Web.HttpApiData qualified as Web

class ToHeader a where
  toHeader :: a -> LBS.ByteString

type Elem :: Exts.Symbol -> [Exts.Symbol] -> Bool
type family Elem x ys where
  Elem x '[] = 'False
  Elem x (x ': ys) = 'True
  Elem x (y ': ys) = Elem x ys

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

data Response where
  Response ::
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
    (ContentType contentType, ToContentType contentType resultType, Typeable.Typeable headerKeys, Typeable.Typeable resultType) =>
    Response
