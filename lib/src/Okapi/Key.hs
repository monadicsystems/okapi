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
{-# LANGUAGE OverloadedRecordDot #-}
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

module Okapi.Key where

import Control.Concurrent.Chan qualified as Chan
import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Tree.Knuth.Forest qualified as Knuth
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App qualified as App
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers
import Okapi.Middleware qualified as Middleware
import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

{-
data Tree (r :: [Type]) where
  Match ::
    forall a (r :: [Type]).
    (Show a, Web.ToHttpApiData a, Eq a, Typeable.Typeable a) =>
    a ->
    Forest r ->
    Tree r
  Param ::
    forall a (r :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Regex ::
    forall a (r :: [Type]).
    (Regex.RegexContext Regex.Regex Text.Text a, Typeable.Typeable a) =>
    Text.Text ->
    Forest (r :-> a) ->
    Tree r
  Splat ::
    forall a (r :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a) =>
    Forest (r :-> NonEmpty.NonEmpty a) ->
    Tree r
  Route ::
    forall a (r :: [Type]).
    (Route.Route a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Query ::
    forall a (r :: [Type]).
    (Query.From a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Headers ::
    forall a (r :: [Type]).
    (Headers.From a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Body ::
    forall a (r :: [Type]).
    (Body.From a, Typeable.Typeable a) =>
    Forest (r :-> a) ->
    Tree r
  Apply ::
    forall t (r :: [Type]).
    (Middleware.Tag t, Eq t, Typeable.Typeable t) =>
    t ->
    Tree r ->
    Tree r
  Responder ::
    forall (status :: Nat.Nat) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type) (r :: [Type]).
    ( Nat.KnownNat status
    , Typeable.Typeable status
    , Response.WaiResponseHeaders headerKeys
    , Response.ContentType contentType
    , Response.ToContentType contentType resultType
    , Typeable.Typeable headerKeys
    , Typeable.Typeable contentType
    , Typeable.Typeable resultType
    ) =>
    Forest (r :-> (Response.Headers headerKeys -> resultType -> Wai.Response)) ->
    Tree r
  -- Events ::
  --   forall (r :: [Type]).
  --   (Typeable.Typeable r) =>
  --   Chan.Chan Wai.ServerEvent ->
  --   Tree r
  Method ::
    forall env (r :: [Type]).
    (Typeable.Typeable env) =>
    HTTP.StdMethod ->
    (env Natural.~> IO) ->
    Handler r env ->
    Tree r
-}

data Key where
    Match :: forall a. (Typeable.Typeable a) => a -> Key
    Param :: forall a. (Typeable.Typeable a) => Key
    Regex :: forall a. (Typeable.Typeable a) => Text.Text -> Key
    Splat :: forall a. (Typeable.Typeable a) => Key
    Route :: forall a. (Typeable.Typeable a) => Key
    Query :: forall a. (Typeable.Typeable a) => Key
    Headers :: forall a. (Typeable.Typeable a) => Key
    Body :: forall a. (Typeable.Typeable a) => Key
    Apply :: forall a. (Typeable.Typeable a) => a -> Key -> Key
    Responder ::
        forall (status :: Nat.Nat) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
        ( Nat.KnownNat status
        , Typeable.Typeable status
        , Typeable.Typeable headerKeys
        , Typeable.Typeable contentType
        , Typeable.Typeable resultType
        ) =>
        Key

-- Method :: HTTP.StdMethod -> Key

unlocks :: Key -> App.Tree r -> Maybe (App.Forest r)
unlocks key tree = case (key, tree) of
    (Match @a1 x1, App.Match @a2 x2 _) -> undefined

-- Returns empty list if the key doesn't have children or the key doesn't exist at all.
-- To differentiate these states, use safeLookup
lookup :: forall r. [Key] -> App.Forest r -> App.Forest r
lookup key forest = undefined

-- Nothing means the key doesn't exist in the forest
-- Just [] means the key exists, but just doesn't have any children
-- Just list means the key exists and has children.
safeLookup :: forall r. [Key] -> App.Forest r -> Maybe (App.Forest r)
safeLookup = undefined
