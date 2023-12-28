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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
import GHC.TypeError qualified as TypeError
import GHC.TypeLits qualified as TypeLits
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers
import Okapi.Kind ((:<|>), (:>))
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

type Root (t :: Kind.Tree) = Tree t '[]

(|||) ::
  forall (t1 :: Kind.Tree) (t2 :: Kind.Tree) (p :: [Type]).
  -- (Kind.NotEqual t1 t2 ~ True) =>
  Tree t1 p ->
  Tree t2 p ->
  Tree (t1 :<|> t2) p
(|||) = Or

type (:<) :: [Type] -> Type -> [Type]
type family (:<) path a = r where
  '[] :< x = x : '[]
  (x : xs) :< x' = x : (xs :< x')

data Tree (t :: Kind.Tree) (p :: [Type]) where
  Lit ::
    forall (s :: Exts.Symbol) (c :: Kind.Tree) (p :: [Type]).
    (TypeLits.KnownSymbol s) =>
    Tree c (p :< Phantom.Lit s) ->
    Tree (Phantom.Lit s :> c) p
  Param ::
    forall (a :: Type) (c :: Kind.Tree) (p :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a) =>
    Tree c (p :< Phantom.Param a) ->
    Tree (Phantom.Param a :> c) p
  Splat ::
    forall a (c :: Kind.Tree) (p :: [Type]).
    (Web.FromHttpApiData a, Typeable.Typeable a) =>
    Tree c (p :< Phantom.Splat a) ->
    Tree (Phantom.Splat a :> c) p
  Route ::
    forall a (c :: Kind.Tree) (p :: [Type]).
    (Route.Route a, Typeable.Typeable a) =>
    Tree c (p :< Phantom.Route a) ->
    Tree (Phantom.Route a :> c) p
  Method ::
    forall (m :: Kind.Method) (env :: Type -> Type) (p :: [Type]).
    (Typeable.Typeable env) =>
    (env Natural.~> IO) ->
    P.Handler p env ->
    Tree (Kind.Leaf m) p
  Responder ::
    forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.Tree) (c :: Kind.Tree) (p :: [Type]).
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
    Tree c (p :< Phantom.Response status headers content result) ->
    Tree t p
  Or ::
    forall (t :: Kind.Tree) (t' :: Kind.Tree) (p :: [Type]).
    -- (Kind.NotEqual t t' ~ True) =>
    Tree t p ->
    Tree t' p ->
    Tree (t :<|> t') p

showType :: forall a. (Typeable.Typeable a) => String
showType = show . Typeable.typeRep $ Typeable.Proxy @a

type To :: Kind.Tree -> [Type] -> Kind.Tree
type family To (tree :: Kind.Tree) (path :: [Type]) where
  To t '[] = t
  To (Kind.Grow n t) (n : ns) = To t ns
  To (Kind.Fork (Kind.Grow n t) _) (n : ns) = To t ns
  To (Kind.Fork t t') ns = To t' ns
  To t p = TypeError.TypeError (TypeError.Text "The Path: " TypeError.:<>: TypeError.ShowType p TypeError.:<>: TypeError.Text " doesn't exist in Tree: " TypeError.:<>: TypeError.ShowType t)

-- type ToP :: Kind.Tree -> [Type] -> [Type]
-- type family ToP (tree :: Kind.Tree) (path :: [Type]) where
--   ToP t '[] = '[]
--   ToP (Kind.Grow n t) (n : ns) = ToP t ns
--   ToP (Kind.Fork (Kind.Grow n t) _) (n : ns) = ToP t ns
--   ToP (Kind.Fork t t') ns = ToP t' ns
--   ToP t p = TypeError.TypeError (TypeError.Text "The Path: " TypeError.:<>: TypeError.ShowType p TypeError.:<>: TypeError.Text " doesn't exist in Tree: " TypeError.:<>: TypeError.ShowType t)
type (+++) :: [Type] -> [Type] -> [Type]
type family (+++) xs ys where
  '[] +++ ys = ys
  xs +++ '[] = xs
  (x : xs) +++ ys = x : xs +++ ys

lookupTree :: (Look tree path '[] tree') => Root tree -> (P.PList '[] -> P.PList path) -> Tree tree' path
lookupTree tree pathF = look tree $ pathF P.go

url :: forall (m :: Kind.Method) tree path. (Look tree path '[] (Kind.Leaf m)) => Root tree -> (P.PList '[] -> P.PList path) -> [Text.Text]
url _ pathF = P.toUrl $ pathF P.go

class
  Look
    (tree :: Kind.Tree)
    (path :: [Type])
    (orig :: [Type])
    (tree' :: Kind.Tree)
  where
  -- type Result tree path :: Type
  look :: Tree tree orig -> P.PList path -> Tree tree' (orig +++ path)

instance
  ( Look c p (o :< Phantom.Lit s) t'
  , (o :< Phantom.Lit s) +++ p ~ o +++ (Phantom.Lit s : p)
  ) =>
  Look (Kind.Grow (Phantom.Lit s) c) (Phantom.Lit s : p) o t'
  where
  look (Lit tree) (P.Lit path) = look tree path

instance
  ( Look c p (o :< Phantom.Param a) t'
  , (o :< Phantom.Param a) +++ p ~ o +++ (Phantom.Param a : p)
  ) =>
  Look (Kind.Grow (Phantom.Param a) c) (Phantom.Param a : p) o t'
  where
  look (Param tree) (P.Param _ path) = look tree path

instance
  ( Look c p (o :< Phantom.Splat a) t'
  , (o :< Phantom.Splat a) +++ p ~ o +++ (Phantom.Splat a : p)
  ) =>
  Look (Kind.Grow (Phantom.Splat a) c) (Phantom.Splat a : p) o t'
  where
  look (Splat tree) (P.Splat _ path) = look tree path

instance
  ( Look c p (o :< Phantom.Route a) t'
  , (o :< Phantom.Route a) +++ p ~ o +++ (Phantom.Route a : p)
  ) =>
  Look (Kind.Grow (Phantom.Route a) c) (Phantom.Route a : p) o t'
  where
  look (Route tree) (P.Route _ path) = look tree path

instance
  ( Look c p (o :< Phantom.Response status headers content result) t'
  , (o :< Phantom.Response status headers content result) +++ p ~ o +++ p
  ) =>
  Look (Kind.Grow (Phantom.Response status headers content result) c) p o t'
  where
  look (Responder tree) = look tree

instance
  ( Look c p (o :< Phantom.Response status headers content result) t'
  , (o :< Phantom.Response status headers content result) +++ p ~ o +++ p
  ) =>
  Look (Kind.Fork (Kind.Grow (Phantom.Response status headers content result) c) x) p o t'
  where
  look (Or (Responder tree) _) = look tree

instance Look (Kind.Fork (Kind.Leaf m) x) '[] o (Kind.Leaf m) where
  look (Or t _) _ = t

instance
  ( Look c p (o :< n) t'
  , (o :< n) +++ p ~ o +++ (n : p)
  ) =>
  Look (Kind.Fork (Kind.Grow n c) x) (n : p) o t'
  where
  look (Or (Lit tree) _) (P.Lit path) = look tree path
  look (Or (Param tree) _) (P.Param _ path) = look tree path
  look (Or (Splat tree) _) (P.Splat _ path) = look tree path
  look (Or (Route tree) _) (P.Route _ path) = look tree path

instance
  {-# OVERLAPPABLE #-}
  ( Look t p o t'
  ) =>
  Look (Kind.Fork x t) p o t'
  where
  look (Or _ tree) = look tree

instance Look (Kind.Leaf m) '[] o (Kind.Leaf m) where
  look tree P.Start = tree

api :: Root _
api = home ||| person

home = Lit @"home" method
 where
  method = Method @Kind.GET id \req -> do undefined

person = (Lit @"person") (withPerson ||| postPerson)

withPerson = (Param @Text.Text) (getPerson ||| pet)

postPerson = Lit @"new" . Param @Text.Text $ Method @Kind.POST id \newPerson req -> do
  undefined

getPerson = Method @Kind.GET id \person req -> do undefined

pet = Lit @"pet" (getPet ||| postPet)

getPet = Param @Text.Text $ Method @Kind.GET id \person pet req -> do
  undefined

postPet = (Lit @"new" . Param @Text.Text) $ Method @Kind.POST id \person newPet req -> do
  undefined

test1 :: Tree ('Kind.Leaf 'Kind.GET) '[Phantom.Lit "home"]
test1 = lookupTree api path
 where
  path = P.Lit @"home"

test2 :: Tree ('Kind.Leaf 'Kind.POST) _
test2 = lookupTree api path
 where
  path =
    P.Lit @"person"
      . P.Param @Text.Text "John"
      . P.Lit @"pet"
      . P.Lit @"new"
      . P.Param @Text.Text "Mochi"

test3 :: Tree ('Kind.Leaf 'Kind.GET) _
test3 = lookupTree api path
 where
  path =
    P.Lit @"person"
      . P.Param @Text.Text "John"
      . P.Lit @"pet"
      . P.Param @Text.Text "Mochi"

test4 :: [Text.Text]
test4 = url @Kind.POST api path
 where
  path =
    P.Lit @"person"
      . P.Param @Text.Text "John"
      . P.Lit @"pet"
      . P.Lit @"new"
      . P.Param @Text.Text "Mochi"

test5 :: [Text.Text]
test5 = url @Kind.GET api path
 where
  path =
    P.Lit @"person"
      . P.Param @Text.Text "Johnny"
