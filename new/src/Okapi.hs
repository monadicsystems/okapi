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

module Okapi where

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
import Data.Type.Bool (type (&&))
import Data.Type.Equality qualified as Equality
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import Data.Void qualified as Void
import GHC.Exts qualified as Exts
import GHC.Generics qualified as Generics
import GHC.TypeError qualified as TypeError
import GHC.TypeLits qualified as TypeLits
import GHC.TypeNats qualified as Nat
import Kind qualified
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Phantom qualified
import Web.HttpApiData qualified as Web

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- type FOREST :: Type
-- type FOREST = [TREE]

-- (+++) :: Forest f p -> Forest f' p -> Forest (f :+++ f') p
-- (+++) x y = case x of
--     Seed -> y
--     (Grow t f) -> Grow t (f +++ y)

type (:<>) :: [k] -> [k] -> [k]
type family (:<>) xs ys where
    '[] :<> ys = ys
    (x : xs) :<> ys = x : xs :<> ys

type (:<) :: [Type] -> Type -> [Type]
type family (:<) xs x where
    '[] :< x = x : '[]
    (x : xs) :< x' = x : (xs :< x')

type (:*) n t = Kind.NODE n t

type (:+) t t' = Kind.BRANCH t t'

type Handler :: [Type] -> (Type -> Type) -> Type
type family Handler p env where
    Handler '[] env = Wai.Request -> env Wai.Response
    Handler (Phantom.Lit s : rem) env = Handler rem env
    Handler (Phantom.Param a : rem) env = a -> Handler rem env
    Handler (Phantom.Splat a : rem) env = NonEmpty.NonEmpty a -> Handler rem env
    Handler (Phantom.Response status headers content result : rem) env = Phantom.Response status headers content result -> Handler rem env
    Handler x _ = TypeError.TypeError (TypeError.Text "Can't create Handler for type: " TypeError.:<>: TypeError.ShowType x)

type Root (t :: Kind.TREE) = Tree t '[]

data Tree (t :: Kind.TREE) (p :: [Type]) where
    Method ::
        forall (v :: Kind.VERB) (env :: Type -> Type) (p :: [Type]).
        (env Natural.~> IO) ->
        Handler p env ->
        Tree (Kind.LEAF v) p
    Lit ::
        forall (s :: Exts.Symbol) (c :: Kind.TREE) (p :: [Type]).
        (TypeLits.KnownSymbol s) =>
        Tree c (p :< Phantom.Lit s) ->
        Tree (Phantom.Lit s :* c) p
    Param ::
        forall (a :: Type) (c :: Kind.TREE) (p :: [Type]).
        (Web.FromHttpApiData a) =>
        Tree c (p :< Phantom.Param a) ->
        Tree (Phantom.Param a :* c) p
    Splat ::
        forall a (c :: Kind.TREE) (p :: [Type]).
        (Web.FromHttpApiData a) =>
        Tree c (p :< Phantom.Splat a) ->
        Tree (Phantom.Splat a :* c) p
    Response ::
        forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.TREE) (c :: Kind.TREE) (p :: [Type]).
        ( Nat.KnownNat status
        , t ~ Phantom.Response status headers content result :* c
        ) =>
        Tree c (p :< Phantom.Response status headers content result) ->
        Tree t p
    Branch ::
        forall (t :: Kind.TREE) (t' :: Kind.TREE) (t'' :: Kind.TREE) (p :: [Type]).
        (t'' ~ t :+ t', Valid t'' ~ True) =>
        Tree t p ->
        Tree t' p ->
        Tree t'' p

method ::
    forall (v :: Kind.VERB) (env :: Type -> Type) (p :: [Type]).
    (env Natural.~> IO) ->
    Handler p env ->
    Tree (Kind.LEAF v) p
method = Method

lit ::
    forall (s :: Exts.Symbol) (c :: Kind.TREE) (p :: [Type]).
    (TypeLits.KnownSymbol s) =>
    Tree c (p :< Phantom.Lit s) ->
    Tree (Phantom.Lit s :* c) p
lit = Lit

param ::
    forall (a :: Type) (c :: Kind.TREE) (p :: [Type]).
    (Web.FromHttpApiData a) =>
    Tree c (p :< Phantom.Param a) ->
    Tree (Phantom.Param a :* c) p
param = Param

splat ::
    forall a (c :: Kind.TREE) (p :: [Type]).
    (Web.FromHttpApiData a) =>
    Tree c (p :< Phantom.Splat a) ->
    Tree (Phantom.Splat a :* c) p
splat = Splat

response ::
    forall (status :: Nat.Nat) (headers :: [Exts.Symbol]) (content :: Type) (result :: Type) (t :: Kind.TREE) (c :: Kind.TREE) (p :: [Type]).
    ( Nat.KnownNat status
    , t ~ Phantom.Response status headers content result :* c
    ) =>
    Tree c (p :< Phantom.Response status headers content result) ->
    Tree t p
response = Response

infixr 6 |||

(|||) ::
    forall (t :: Kind.TREE) (t' :: Kind.TREE) (t'' :: Kind.TREE) (p :: [Type]).
    (t'' ~ t :+ t', Valid t'' ~ True) =>
    Tree t p ->
    Tree t' p ->
    Tree t'' p
(|||) = Branch

type Valid :: Kind.TREE -> Bool
type family Valid api where
    Valid t = ValidHelper '[] '[] t

type ValidHelper :: [Type] -> [Kind.VERB] -> Kind.TREE -> Bool
type family ValidHelper nodes leaves t where
    ValidHelper seenNodes seenLeaves (Kind.LEAF v) = NotElem v seenLeaves
    ValidHelper seenNodes seenLeaves (Kind.NODE n st) =
        NotElem n seenNodes && Valid st
    ValidHelper seenNodes seenLeaves (Kind.BRANCH (Kind.LEAF v) t') =
        NotElem v seenLeaves && ValidHelper seenNodes (v : seenLeaves) t'
    ValidHelper seenNodes seenLeaves (Kind.BRANCH (Kind.NODE n st) t') =
        NotElem n seenNodes && Valid st && ValidHelper (n : seenNodes) seenLeaves t'
    ValidHelper seenNodes seenLeaves (Kind.BRANCH t t') =
        ValidHelper seenNodes seenLeaves t && ValidHelper seenNodes seenLeaves t'

type NotElem :: k -> [k] -> Bool
type family NotElem k ks where
    NotElem k '[] = True
    NotElem k (k : ks) = False
    NotElem k (k' : ks) = True && NotElem k ks

type Endpoints :: Kind.TREE -> [([Type], Kind.VERB)]
type family Endpoints api where
    Endpoints (Kind.BRANCH a b) = Endpoints a :<> Endpoints b
    Endpoints (Kind.NODE n a) = MapSub n (Endpoints a)
    Endpoints (Kind.LEAF v) = '[ '( '[], v)]

type MapSub :: Type -> [([Type], Kind.VERB)] -> [([Type], Kind.VERB)]
type family MapSub e xs where
    MapSub e '[] = '[]
    MapSub e ('(ts, v) ': xs) = '(e ': ts, v) ': MapSub e xs

api :: Root _
api = home ||| person

home =
    lit @"hello"
        . lit @"world"
        . param @Text.Text
        $ method @Kind.GET @IO id \(name :: Text.Text) req -> do
            undefined

person = lit @"person" $ method @Kind.PUT @IO id \_ -> undefined
