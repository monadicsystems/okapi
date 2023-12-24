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

-- {-# HLINT ignore "Use if" #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Okapi.App where

import Control.Arrow ((>>>))
import Control.Concurrent.Chan qualified as Chan
import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.Bits (Bits (testBit))
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
import Okapi.Kind ((:-), (:<|>))
import Okapi.Kind qualified as Kind
import Okapi.Middleware qualified as Middleware
import Okapi.P qualified as P
import Okapi.Phantom qualified as Phantom
import Okapi.Q qualified as Q
import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

type Root (t :: Kind.Tree) = Tree t P.Root

-- (.>) mkTree1 mkTree2 = mkTree2 . mkTree1

(.<|>) ::
  forall (t1 :: Kind.Tree) (t2 :: Kind.Tree) (p :: P.P).
  -- (Kind.NotEqual t1 t2 ~ True) =>
  Tree t1 p ->
  Tree t2 p ->
  Tree (t1 :<|> t2) p
(.<|>) = Fork

type Test =
  P.Handler
    ( ( (('P.Root 'P.:-> Phantom.Lit "hello") 'P.:-> Phantom.Lit "world")
          'P.:-> Phantom.Param Float
      )
        'P.:-> Phantom.Param Char
    )
    IO

test :: Root _
test = Lit @"hello" . Lit @"world" . Param @Float . Param @Char $ testMethods

test2 :: Root _
test2 = (Lit @"hello" . Lit @"world" . Param @Float . Param @Char . Method @Kind.PUT @IO id) \(f :: Float) (c :: Char) (r :: Wai.Request) -> do
  undefined

tester = mkURL test P.testP

testMethods = method1 .<|> method2 .<|> method3
 where
  method1 = Method @Kind.GET @IO id \(f :: Float) (c :: Char) (r :: Wai.Request) -> do undefined
  method2 = Method @Kind.POST @IO id undefined
  method3 = Method @Kind.PUT @IO id undefined

-- testURL = mkURL test P.testP

data Tree (t :: Kind.Tree) (p :: P.P) where
  Lit ::
    forall (s :: Exts.Symbol) (t :: Kind.Tree) (c :: Kind.Tree) (p :: P.P).
    (TypeLits.KnownSymbol s, t ~ Phantom.Lit s :- c) =>
    Tree c (p P.:-> Phantom.Lit s) ->
    Tree t p
  Param ::
    forall (a :: Type) (t :: Kind.Tree) (c :: Kind.Tree) (p :: P.P).
    (Web.FromHttpApiData a, Typeable.Typeable a, t ~ Phantom.Param a :- c) =>
    Tree c (p P.:-> Phantom.Param a) ->
    Tree t p
  Splat ::
    forall a (t :: Kind.Tree) (c :: Kind.Tree) (p :: P.P).
    (Web.FromHttpApiData a, Typeable.Typeable a, t ~ Phantom.Splat a :- c) =>
    Tree c (p P.:-> Phantom.Splat a) ->
    Tree t p
  Route ::
    forall a (t :: Kind.Tree) (c :: Kind.Tree) (p :: P.P).
    (Route.Route a, Typeable.Typeable a, t ~ Phantom.Route a :- c) =>
    Tree c (p P.:-> Phantom.Route a) ->
    Tree t p
  Method ::
    forall (m :: Kind.Method) (env :: Type -> Type) (p :: P.P).
    (Typeable.Typeable env) =>
    (env Natural.~> IO) ->
    P.Handler (P.Reverse p) env ->
    Tree (Kind.Leaf m) p
  Responder ::
    forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.Tree) (c :: Kind.Tree) (p :: P.P).
    ( Nat.KnownNat status
    , Typeable.Typeable status
    , Response.WaiResponseHeaders headers
    , Response.ContentType content
    , Response.ToContentType content result
    , Typeable.Typeable headers
    , Typeable.Typeable content
    , Typeable.Typeable result
    , t ~ Phantom.Response status headers content result :- c
    ) =>
    Tree c (p P.:-> Phantom.Response status headers content result) ->
    Tree t p
  Fork ::
    forall (t :: Kind.Tree) (t' :: Kind.Tree) (p :: P.P).
    -- (Kind.NotEqual t t' ~ True) =>
    Tree t p ->
    Tree t' p ->
    Tree (t :<|> t') p

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

mkURL :: (URL t (P.Reverse p)) => Tree t p' -> P.PList p -> [Text.Text]
mkURL tree = url tree . P.flipper

class URL (t :: Kind.Tree) (p :: P.P) where
  url :: Tree t p' -> P.PList p -> [Text.Text]

instance (URL t p) => URL (Phantom.Lit s :- t) (p P.:-> Phantom.Lit s) where
  url (Lit child) (p P.:/= Phantom.Lit) = Text.pack (TypeLits.symbolVal @s Typeable.Proxy) : url child p

instance (URL t p) => URL (Phantom.Param a :- t) (p P.:-> Phantom.Param a) where
  url (Param child) (p P.:/: a) = Web.toUrlPiece a : url child p

instance (URL t p) => URL (Phantom.Splat a :- t) (p P.:-> Phantom.Splat a) where
  url (Splat child) (p P.:/* nel) = NonEmpty.toList (Web.toUrlPiece <$> nel) <> url child p

instance (URL t p) => URL (Phantom.Route a :- t) (p P.:-> Phantom.Route a) where
  url (Route child) (p P.:/# a) = Route.printer a <> url child p

instance (URL t p) => URL (Phantom.Response status headers content result :- t) p where
  url (Responder child) p = url child p

instance (URL (ph :- c) (pt P.:-> ph)) => URL ((ph :- c) :<|> t') (pt P.:-> ph) where
  url (Fork t _) = url t

instance (URL (ph :- c) (pt P.:-> ph)) => URL (t' :<|> (ph :- c)) (pt P.:-> ph) where
  url (Fork _ t) = url t

instance URL (Kind.Leaf m) P.Root where
  url (Method _ _) _ = [] :: [Text.Text]

instance URL (Kind.Leaf m :<|> t) P.Root where
  url _ _ = [] :: [Text.Text]

instance URL (t :<|> Kind.Leaf m) P.Root where
  url _ _ = [] :: [Text.Text]
