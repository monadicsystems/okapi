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
import Data.Aeson qualified as Aeson
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Functor.Identity qualified as Identity
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as Text
import Data.Tree qualified as Tree
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.Natural qualified as Natural
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Headers qualified as Headers
import Okapi.Route qualified as Route

import Web.HttpApiData qualified as Web

class ToHeader a where
  toHeader :: a -> LBS.ByteString

instance ToHeader LBS.ByteString where
  toHeader = id

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

noHeaders :: Headers '[]
noHeaders = NoHeaders

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
  | BodyFile FilePath (Maybe Wai.FilePart)

class ContentType a where
  contentTypeName :: LBS.ByteString
  contentTypeBody :: a -> Body

instance ContentType Text.Text where
  contentTypeName = "text/plain"
  contentTypeBody = BodyBytes . Text.encodeUtf8 . LText.fromStrict

instance ContentType Aeson.Value where
  contentTypeName = "application/json"
  contentTypeBody = BodyBytes . Aeson.encode

class (ContentType a) => ToContentType a b where
  toContentType :: b -> a

instance ToContentType Text.Text Text.Text where
  toContentType = id

instance ToContentType Text.Text Int where
  toContentType = Text.pack . show

instance (Aeson.ToJSON a) => ToContentType Aeson.Value a where
  toContentType = Aeson.toJSON

data Response where
  Response ::
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
    (ContentType contentType, ToContentType contentType resultType, Typeable.Typeable headerKeys, Typeable.Typeable resultType) =>
    Response

toWaiResponseHeaders ::
  forall (headerKeys :: [Exts.Symbol]).
  Headers headerKeys ->
  HTTP.ResponseHeaders
toWaiResponseHeaders headers = []

natToStatus :: Nat.Nat -> HTTP.Status
natToStatus n = toEnum $ fromEnum n

makeResponder ::
  forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
  (Nat.KnownNat status, ContentType contentType, ToContentType contentType resultType, Typeable.Typeable headerKeys, Typeable.Typeable resultType) =>
  (Headers headerKeys -> resultType -> Wai.Response)
makeResponder headerMap result =
  let status = natToStatus $ Nat.natVal @status Typeable.Proxy
      contentType = toContentType @contentType @resultType result
      bodyType = contentTypeBody @contentType contentType
      name = contentTypeName @contentType
      headers = toWaiResponseHeaders headerMap
   in case bodyType of
        BodyBytes bytes -> Wai.responseLBS status headers bytes
        BodyBuilder builder -> Wai.responseBuilder status headers builder
        BodyStream stream -> Wai.responseStream status headers stream
        BodyFile path part -> Wai.responseFile status headers path part
