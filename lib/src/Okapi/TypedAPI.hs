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

module Okapi.API where

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
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Headers qualified as Headers
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Web.HttpApiData qualified as Web

module Okapi.TypedAPI where

type MethodKind :: Type
data MethodKind where
  GETType :: MethodKind
  POSTType :: MethodKind
  PUTType :: MethodKind
  DELETEType :: MethodKind

type OpTree :: Type
data OpTree where
  MatchNode :: forall a. (Web.ToHttpApiData a) => a -> '[TypedAPI OpTree] -> OpTree
  ParamNode :: forall a. (Web.FromHttpApiData a) => '[TypedAPI OpTree] -> OpTree
  MethodLeaf :: MethodKind -> OpTree

-- type In :: TypedAPI OpTree -> [TypedAPI OpTree] -> Bool
-- type family In (MatchNode ) (t :: [TypedAPI OpTree]) where
--   Pop '[] =

type OpTreee :: OpTree -> Type
data OpTreee where
  OpTreee :: OpTree -> OpTreee

data TypedAPI (t :: OpTreee) where
  Match' :: forall a t. (Web.ToHttpApiData a, t ~ [TypedAPI OpTreee]) => a -> t -> TypedAPI (OpTreee (MatchNode a t))
  Param' :: forall a t. (Web.FromHttpApiData a, Typeable.Typeable a, t ~ [TypedAPI OpTreee]) => (Secret.Secret a -> t) -> TypedAPI (OpTreee (ParamNode a t))
  Method' :: forall (m :: MethodKind) env. (env Natural.~> IO) -> Handler env -> TypedAPI (OpTreee (MethodLeaf m))