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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Okapi.App where

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
import Data.Void qualified as Void
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.TypeLits qualified as TypeLits
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers
import Okapi.Kind (Path ((:->)), (:<|>), (:>))
import Okapi.Kind qualified as Kind
import Okapi.Middleware qualified as Middleware
import Okapi.Phantom qualified as Phantom
import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

type Root (t :: Kind.Tree) = Tree t Kind.Root

(.>) :: (Tree t1 p1 -> Tree t2 p2) -> (Tree t2 p2 -> Tree t3 p3) -> Tree t1 p1 -> Tree t3 p3
(.>) mkTree1 mkTree2 = mkTree2 . mkTree1

(.<|>) ::
  forall (t1 :: Kind.Tree) (t2 :: Kind.Tree) (p :: Kind.Path).
  (Kind.NotEqual t1 t2 ~ True) =>
  Tree t1 p ->
  Tree t2 p ->
  Tree (t1 :<|> t2) p
(.<|>) = Fork

test :: Root _
test = Lit @"hello" .> Lit @"world" .> Param @Float $ testMethods

testMethods = method1 .<|> method2 .<|> method3
 where
  method1 = Method @Kind.GET @IO id \(f :: Float) -> do undefined
  method2 = Method @Kind.POST @IO id \(f :: Float) -> do undefined
  method3 = Method @Kind.GET @IO id \(f :: Float) (i :: Int) _ -> do undefined


data Tree (t :: Kind.Tree) (p :: Kind.Path) where
  Lit ::
    forall (s :: Exts.Symbol) (t :: Kind.Tree) (c :: Kind.Tree) (p :: Kind.Path).
    (TypeLits.KnownSymbol s, t ~ Phantom.Lit s :> c) =>
    Tree c (p :-> Phantom.Lit s) ->
    Tree t p
  Param ::
    forall (a :: Type) (t :: Kind.Tree) (c :: Kind.Tree) (p :: Kind.Path).
    (Web.FromHttpApiData a, Typeable.Typeable a, t ~ Phantom.Param a :> c) =>
    Tree c (p :-> Phantom.Param a) ->
    Tree t p
  Splat ::
    forall a (t :: Kind.Tree) (c :: Kind.Tree) (p :: Kind.Path).
    (Web.FromHttpApiData a, Typeable.Typeable a, t ~ Phantom.Splat a :> c) =>
    Tree c (p :-> Phantom.Splat a) ->
    Tree t p
  Route ::
    forall a (t :: Kind.Tree) (c :: Kind.Tree) (p :: Kind.Path).
    (Route.Route a, Typeable.Typeable a, t ~ Phantom.Route a :> c) =>
    Tree c (p :-> Phantom.Route a) ->
    Tree t p
  Method ::
    forall (m :: Kind.Method) (env :: Type -> Type) (p :: Kind.Path).
    (Typeable.Typeable env) =>
    (env Natural.~> IO) ->
    Kind.Handler p env ->
    Tree (Kind.Leaf m) p
  Responder ::
    forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.Tree) (c :: Kind.Tree) (p :: Kind.Path).
    ( Nat.KnownNat status
    , Typeable.Typeable status
    , Response.WaiResponseHeaders headers
    , Response.ContentType content
    , Response.ToContentType content result
    , Typeable.Typeable headers
    , Typeable.Typeable content
    , Typeable.Typeable result
    , t ~ Phantom.Response status headers content result :> c
    ) =>
    Tree c (p :-> Phantom.Response status headers content result) ->
    Tree t p
  Fork ::
    forall (t :: Kind.Tree) (t' :: Kind.Tree) (p :: Kind.Path).
    (Kind.NotEqual t t' ~ True) =>
    Tree t p ->
    Tree t' p ->
    Tree (t :<|> t') p

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a
