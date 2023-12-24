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
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Okapi.Kind where

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
import GHC.TypeError qualified as TypeError
import GHC.TypeLits qualified as TypeLits
import GHC.TypeNats qualified as Nat
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Body qualified as Body
import Okapi.Headers qualified as Headers
import Okapi.Middleware qualified as Middleware
import Okapi.Phantom qualified as Phantom
import Okapi.Query qualified as Query
import Okapi.Response qualified as Response
import Okapi.Route qualified as Route
import Text.Regex.TDFA qualified as Regex
import Web.HttpApiData qualified as Web

type NotEqual :: Tree -> Tree -> Bool
type family NotEqual a b where
  NotEqual a a = False
  NotEqual a b = True

type Path :: Type
data Path where
  Root :: Path
  (:->) :: Path -> Type -> Path

type PList :: Path -> Type
data PList (p :: Path) where
  Go :: PList Root
  (:/=) :: (TypeLits.KnownSymbol s) => PList p -> Phantom.Lit s -> PList (p :-> Phantom.Lit s)
  (:/:) :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => PList p -> a -> PList (p :-> Phantom.Param a)
  (:/*) :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => PList p -> NonEmpty.NonEmpty a -> PList (p :-> Phantom.Splat a)
  (:/#) :: (Route.Route a) => PList p -> a -> PList (p :-> Phantom.Route a)
  (:>>) :: (TypeLits.KnownNat status, Response.WaiResponseHeaders headers, Response.ToContentType content result) => PList p -> Phantom.Response status headers content result -> PList (p :-> Phantom.Response status headers content result)

path :: PList Root
path = Go

testGo =
  path
    :/= Phantom.Lit @"hello"
    :/* NonEmpty.fromList [1, 2, 3 :: Int]
    :/= Phantom.Lit @"world"
    :/: ("myName" :: Text.Text)
    :/: (5 :: Int)

type Handler :: Path -> (Type -> Type) -> Type
type family Handler path env where
  Handler Root env = Wai.Request -> env Wai.Response
  Handler (rem :-> Phantom.Lit s) env = Handler rem env
  Handler (rem :-> Phantom.Param a) env = a -> Handler rem env
  Handler (rem :-> Phantom.Splat a) env = NonEmpty.NonEmpty a -> Handler rem env
  Handler (rem :-> Phantom.Route a) env = a -> Handler rem env
  Handler (rem :-> Phantom.Response status headers content result) env = (Response.Headers headers -> result -> Wai.Response) -> Handler rem env
  Handler x _ = TypeError.TypeError (TypeError.Text "Can't create Handler for type: " TypeError.:<>: TypeError.ShowType x)

-- snoc :: forall (l :: [Type]) (e :: Type). (Renderable e) => HList l -> e -> HList (l :-> e)
-- snoc HNil x = HCons x HNil
-- snoc (HCons h t) x = HCons h (snoc t x)

runHandler :: Handler p env -> PList p -> (Wai.Request -> env Wai.Response)
runHandler handler Go = handler
runHandler handler (rest :/= lit) = runHandler handler rest
runHandler handler (rest :/: param) = runHandler (handler param) rest
runHandler handler (rest :/* splat) = runHandler (handler splat) rest
runHandler handler (rest :/# route) = runHandler (handler route) rest
runHandler handler (rest :>> Phantom.Response @status @headers @content @result) = runHandler (handler $ Response.makeResponder @status @headers @content @result) rest

type Tree :: Type
data Tree where
  Leaf :: Method -> Tree
  Grow :: Type -> Tree -> Tree
  Fork :: Tree -> Tree -> Tree

type n :> t = Grow n t

type t :<|> t' = Fork t t'

type Method :: Type
data Method where
  GET :: Method
  POST :: Method
  PUT :: Method
