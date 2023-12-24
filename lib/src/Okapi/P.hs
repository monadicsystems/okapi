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

module Okapi.P where

import Control.Concurrent.Chan qualified as Chan
import Control.Natural qualified as Natural
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
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

type P :: Type
data P where
    Root :: P
    (:->) :: P -> Type -> P

type PList :: P -> Type
data PList (p :: P) where
    Start :: PList Root
    (:/=) :: (TypeLits.KnownSymbol s) => PList p -> Phantom.Lit s -> PList (p :-> Phantom.Lit s)
    (:/:) :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => PList p -> a -> PList (p :-> Phantom.Param a)
    (:/*) :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => PList p -> NonEmpty.NonEmpty a -> PList (p :-> Phantom.Splat a)
    (:/#) :: (Route.Route a) => PList p -> a -> PList (p :-> Phantom.Route a)
    (:>>) :: (TypeLits.KnownNat status, Response.WaiResponseHeaders headers, Response.ToContentType content result) => PList p -> Phantom.Response status headers content result -> PList (p :-> Phantom.Response status headers content result)

go :: PList Root
go = Start

testP =
    go
        :/= Phantom.Lit @"hello"
        :/= Phantom.Lit @"world"
        :/: (9008.609 :: Float)
        :/: 'y'

type Append :: P -> P -> P
type family Append p p' where
    Append x Root = x
    Append Root x = x
    Append (as :-> a) (bs :-> b) = Append (as :-> a) bs :-> b

testAppend = append testP testP

append :: PList p -> PList p' -> PList (Append p p')
append Start x = x
append x Start = x
-- Lit
append (x :/= Phantom.Lit @s') (y :/= Phantom.Lit @s) = append (x :/= Phantom.Lit @s') y :/= Phantom.Lit @s
append (x :/: a) (y :/= Phantom.Lit @s) = append (x :/: a) y :/= Phantom.Lit @s
append (x :/* nel) (y :/= Phantom.Lit @s) = append (x :/* nel) y :/= Phantom.Lit @s
append (x :/# r) (y :/= Phantom.Lit @s) = append (x :/# r) y :/= Phantom.Lit @s
append (x :>> res) (y :/= Phantom.Lit @s) = append (x :>> res) y :/= Phantom.Lit @s
-- Param
append (x :/= Phantom.Lit @s) (y :/: a) = append (x :/= Phantom.Lit @s) y :/: a
append (x :/: a') (y :/: a) = append (x :/: a') y :/: a
append (x :/* nel) (y :/: a) = append (x :/* nel) y :/: a
append (x :/# r) (y :/: a) = append (x :/# r) y :/: a
append (x :>> res) (y :/: a) = append (x :>> res) y :/: a
-- Splat
append (x :/= Phantom.Lit @s) (y :/* nel) = append (x :/= Phantom.Lit @s) y :/* nel
append (x :/: a) (y :/* nel) = append (x :/: a) y :/* nel
append (x :/* nel') (y :/* nel) = append (x :/* nel') y :/* nel
append (x :/# r) (y :/* nel) = append (x :/# r) y :/* nel
append (x :>> res) (y :/* nel) = append (x :>> res) y :/* nel
-- Route
append (x :/= Phantom.Lit @s) (y :/# r) = append (x :/= Phantom.Lit @s) y :/# r
append (x :/: a) (y :/# r) = append (x :/: a) y :/# r
append (x :/* nel') (y :/# r) = append (x :/* nel') y :/# r
append (x :/# r') (y :/# r) = append (x :/# r') y :/# r
append (x :>> res) (y :/# r) = append (x :>> res) y :/# r
-- Response
append (x :/= Phantom.Lit @s) (y :>> res) = append (x :/= Phantom.Lit @s) y :>> res
append (x :/: a) (y :>> res) = append (x :/: a) y :>> res
append (x :/* nel') (y :>> res) = append (x :/* nel') y :>> res
append (x :/# r') (y :>> res) = append (x :/# r') y :>> res
append (x :>> res') (y :>> res) = append (x :>> res') y :>> res

type Reverse :: P -> P
type family Reverse p where
    Reverse Root = Root
    Reverse (Root :-> h) = (Root :-> h)
    Reverse (t :-> x) = Append (Root :-> x) (Reverse t)

type Handler :: P -> (Type -> Type) -> Type
type family Handler p env where
    Handler Root env = Wai.Request -> env Wai.Response
    Handler (rem :-> Phantom.Lit s) env = Handler rem env
    Handler (rem :-> Phantom.Param a) env = a -> Handler rem env
    Handler (rem :-> Phantom.Splat a) env = NonEmpty.NonEmpty a -> Handler rem env
    Handler (rem :-> Phantom.Route a) env = a -> Handler rem env
    Handler (rem :-> Phantom.Response status headers content result) env = (Response.Headers headers -> result -> Wai.Response) -> Handler rem env
    Handler x _ = TypeError.TypeError (TypeError.Text "Can't create Handler for type: " TypeError.:<>: TypeError.ShowType x)

-- snoc :: forall (p :: P) (a :: Type). a -> PList p -> PList (p :-> a)
-- snoc x Start = Start :-> x
-- snoc x (t :/= h) = HCons h (snoc t x)

flipper :: PList p -> PList (Reverse p)
flipper Start = Start
-- Lit
flipper (Start :/= Phantom.Lit @s) = Start :/= Phantom.Lit
flipper ((t :/= Phantom.Lit @s') :/= Phantom.Lit @s) = append (Start :/= Phantom.Lit @s) (flipper (t :/= Phantom.Lit @s'))
flipper ((t :/: a) :/= Phantom.Lit @s) = append (Start :/= Phantom.Lit @s) (flipper (t :/: a))
flipper ((t :/* nel) :/= Phantom.Lit @s) = append (Start :/= Phantom.Lit @s) (flipper (t :/* nel))
flipper ((t :/# r) :/= Phantom.Lit @s) = append (Start :/= Phantom.Lit @s) (flipper (t :/# r))
flipper ((t :>> res) :/= Phantom.Lit @s) = append (Start :/= Phantom.Lit @s) (flipper (t :>> res))
-- Param
flipper (Start :/: a) = Start :/: a
flipper ((t :/= Phantom.Lit @s) :/: a) = append (Start :/: a) (flipper (t :/= Phantom.Lit @s))
flipper ((t :/: a') :/: a) = append (Start :/: a) (flipper (t :/: a'))
flipper ((t :/* nel) :/: a) = append (Start :/: a) (flipper (t :/* nel))
flipper ((t :/# r) :/: a) = append (Start :/: a) (flipper (t :/# r))
flipper ((t :>> res) :/: a) = append (Start :/: a) (flipper (t :>> res))
-- Splat
flipper (Start :/* nel) = Start :/* nel
flipper ((t :/= Phantom.Lit @s) :/* nel) = append (Start :/* nel) (flipper (t :/= Phantom.Lit @s))
flipper ((t :/: a) :/* nel) = append (Start :/* nel) (flipper (t :/: a))
flipper ((t :/* nel') :/* nel) = append (Start :/* nel) (flipper (t :/* nel'))
flipper ((t :/# r) :/* nel) = append (Start :/* nel) (flipper (t :/# r))
flipper ((t :>> res) :/* nel) = append (Start :/* nel) (flipper (t :>> res))
-- Route
flipper (Start :/# r) = Start :/# r
flipper ((t :/= Phantom.Lit @s) :/# r) = append (Start :/# r) (flipper (t :/= Phantom.Lit @s))
flipper ((t :/: a) :/# r) = append (Start :/# r) (flipper (t :/: a))
flipper ((t :/* nel) :/# r) = append (Start :/# r) (flipper (t :/* nel))
flipper ((t :/# r') :/# r) = append (Start :/# r) (flipper (t :/# r'))
flipper ((t :>> res) :/# r) = append (Start :/# r) (flipper (t :>> res))
-- Response
flipper (Start :>> res) = Start :>> res
flipper ((t :/= Phantom.Lit @s) :>> res) = append (Start :>> res) (flipper (t :/= Phantom.Lit @s))
flipper ((t :/: a) :>> res) = append (Start :>> res) (flipper (t :/: a))
flipper ((t :/* nel) :>> res) = append (Start :>> res) (flipper (t :/* nel))
flipper ((t :/# r) :>> res) = append (Start :>> res) (flipper (t :/# r))
flipper ((t :>> res') :>> res) = append (Start :>> res) (flipper (t :>> res'))

runHandler :: Handler p env -> PList p -> (Wai.Request -> env Wai.Response)
runHandler handler Start = handler
runHandler handler (rest :/= lit) = runHandler handler rest
runHandler handler (rest :/: param) = runHandler (handler param) rest
runHandler handler (rest :/* splat) = runHandler (handler splat) rest
runHandler handler (rest :/# route) = runHandler (handler route) rest
runHandler handler (rest :>> Phantom.Response @status @headers @content @result) = runHandler (handler $ Response.makeResponder @status @headers @content @result) rest
