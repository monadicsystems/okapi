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

module Okapi.Q where

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

type Q :: Type
data Q where
    Tip :: Q
    (:<-) :: Type -> Q -> Q

type QList :: Q -> Type
data QList (q :: Q) where
    End :: QList Tip
    (:/=) :: (TypeLits.KnownSymbol s) => Phantom.Lit s -> QList q -> QList (Phantom.Lit s :<- q)
    (:/:) :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => a -> QList q -> QList (Phantom.Param a :<- q)
    (:/*) :: (Web.FromHttpApiData a, Web.ToHttpApiData a) => NonEmpty.NonEmpty a -> QList q -> QList (Phantom.Splat a :<- q)
    (:/#) :: (Route.Route a) => a -> QList q -> QList (Phantom.Route a :<- q)
    (:>>) :: (TypeLits.KnownNat status, Response.WaiResponseHeaders headers, Response.ToContentType content result) => Phantom.Response status headers content result -> QList q -> QList (Phantom.Response status headers content result :<- q)

end :: QList Tip
end = End

type Handler :: Q -> (Type -> Type) -> Type
type family Handler q env where
    Handler Tip env = Wai.Request -> env Wai.Response
    Handler (Phantom.Lit s :<- rem) env = Handler rem env
    Handler (Phantom.Param a :<- rem) env = a -> Handler rem env
    Handler (Phantom.Splat a :<- rem) env = NonEmpty.NonEmpty a -> Handler rem env
    Handler (Phantom.Route a :<- rem) env = a -> Handler rem env
    Handler (Phantom.Response status headers content result :<- rem) env = (Response.Headers headers -> result -> Wai.Response) -> Handler rem env
    Handler x _ = TypeError.TypeError (TypeError.Text "Can't create Handler for type: " TypeError.:<>: TypeError.ShowType x)

-- snoc :: forall (l :: [Type]) (e :: Type). (Renderable e) => HList l -> e -> HList (l :-> e)
-- snoc HNil x = HCons x HNil
-- snoc (HCons h t) x = HCons h (snoc t x)

runHandler :: Handler q env -> QList q -> (Wai.Request -> env Wai.Response)
runHandler handler End = handler
runHandler handler (lit :/= rest) = runHandler handler rest
runHandler handler (param :/: rest) = runHandler (handler param) rest
runHandler handler (splat :/* rest) = runHandler (handler splat) rest
runHandler handler (route :/# rest) = runHandler (handler route) rest
runHandler handler (Phantom.Response @status @headers @content @result :>> rest) = runHandler (handler $ Response.makeResponder @status @headers @content @result) rest
