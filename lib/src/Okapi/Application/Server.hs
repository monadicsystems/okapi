{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Application.Server (
    Server (..),
    fn,
    type (~>),
    serve,
    tryServe,
    GServer,
    app,
) where

import Data.Kind (Type)
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Codec (Value)
import Okapi.Protocol.Request (Request)
import Okapi.Protocol.Response (Cases)
import Okapi.Contract (Signature)
import Okapi.Contract (Contract, parseRequest, printResponse)

data Server (n :: Type -> Type) sig where
    Fn ::
        ((Request Value method path query headers body, Wai.Request) -> n (responses Value)) ->
        Server n (Signature method path query headers body responses)

fn ::
    ((Request Value method path query headers body, Wai.Request) -> n (responses Value)) ->
    Server n (Signature method path query headers body responses)
fn = Fn

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

serve ::
    Cases responses =>
    (n ~> IO) ->
    Contract (Signature method path query headers body responses) ->
    Server n (Signature method path query headers body responses) ->
    Wai.Application
serve runner endpoint (Fn handler) waiReq respond = do
    parsed <- parseRequest endpoint waiReq
    case parsed of
        Left _    -> respond (Wai.responseLBS HTTP.status400 [] mempty)
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

tryServe ::
    Cases responses =>
    (n ~> IO) ->
    Contract (Signature method path query headers body responses) ->
    Server n (Signature method path query headers body responses) ->
    Wai.Middleware
tryServe runner endpoint (Fn handler) next waiReq respond = do
    parsed <- parseRequest endpoint waiReq
    case parsed of
        Left _    -> next waiReq respond
        Right req -> do
            resVal <- runner (handler (req, waiReq))
            waiRes <- printResponse endpoint resVal
            respond waiRes

class GServer (n :: Type -> Type) (epF :: Type -> Type) (svF :: Type -> Type) where
    gServe :: (n ~> IO) -> epF () -> svF () -> Wai.Middleware

instance GServer n epF svF => GServer n (D1 dm epF) (D1 dm' svF) where
    gServe runner (M1 ep) (M1 sv) = gServe @n @epF @svF runner ep sv

instance GServer n epF svF => GServer n (C1 cm epF) (C1 cm' svF) where
    gServe runner (M1 ep) (M1 sv) = gServe @n @epF @svF runner ep sv

instance (GServer n epL svL, GServer n epR svR)
    => GServer n (epL :*: epR) (svL :*: svR) where
    gServe runner (epL :*: epR) (svL :*: svR) =
        gServe @n @epL @svL runner epL svL
        . gServe @n @epR @svR runner epR svR

instance Cases responses => GServer n
    (S1 sm  (Rec0 (Contract (Signature method path query headers body responses))))
    (S1 sm' (Rec0 (Server n (Signature method path query headers body responses)))) where
    gServe runner (M1 (K1 ep)) (M1 (K1 sv)) = tryServe runner ep sv

app ::
    forall server n.
    ( Generic (server Contract)
    , Generic (server (Server n))
    , GServer n (Rep (server Contract)) (Rep (server (Server n)))
    ) =>
    server Contract ->
    (n ~> IO) ->
    server (Server n) ->
    Wai.Application
app endpoints runner handlers =
    gServe @n @(Rep (server Contract)) @(Rep (server (Server n)))
        runner
        (from endpoints)
        (from handlers)
    $ \_ respond -> respond (Wai.responseLBS HTTP.status404 [] mempty)
