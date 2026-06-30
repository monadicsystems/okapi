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

module Okapi.Forest.Server (
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
import Okapi.Forest.Canopy (Canopy (..), Signature)
import Okapi.Mode.Data qualified as Data
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Responses (Only (..))
import Okapi.HTTP.Responses qualified as Resps

data Server (n :: Type -> Type) sig where
    Fn ::
        ((Data.Request method path query headers body, Wai.Request) -> n (responses Data.Response)) ->
        Server n (Signature method path query headers body responses)

fn ::
    ((Data.Request method path query headers body, Wai.Request) -> n (responses Data.Response)) ->
    Server n (Signature method path query headers body responses)
fn = Fn

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

serve ::
    (n ~> IO) ->
    Canopy (Signature method path query headers body responses) ->
    Server n (Signature method path query headers body responses) ->
    Wai.Application
serve runner endpoint (Fn handler) waiReq respond = case endpoint of
    (req :-> singleRes) -> do
        parsed <- Req.parseRequest req waiReq
        case parsed of
            Left _       -> respond (Wai.responseLBS HTTP.status400 [] mempty)
            Right reqVal -> do
                Only resVal <- runner (handler (reqVal, waiReq))
                waiRes <- Res.printResponse singleRes resVal
                respond waiRes
    (req :-< resContracts) -> do
        parsed <- Req.parseRequest req waiReq
        case parsed of
            Left _       -> respond (Wai.responseLBS HTTP.status400 [] mempty)
            Right reqVal -> do
                resVal <- runner (handler (reqVal, waiReq))
                waiRes <- Resps.printResponses resContracts resVal
                respond waiRes

tryServe ::
    (n ~> IO) ->
    Canopy (Signature method path query headers body responses) ->
    Server n (Signature method path query headers body responses) ->
    Wai.Middleware
tryServe runner endpoint (Fn handler) next waiReq respond = case endpoint of
    (req :-> singleRes) -> do
        parsed <- Req.parseRequest req waiReq
        case parsed of
            Left _       -> next waiReq respond
            Right reqVal -> do
                Only resVal <- runner (handler (reqVal, waiReq))
                waiRes <- Res.printResponse singleRes resVal
                respond waiRes
    (req :-< resContracts) -> do
        parsed <- Req.parseRequest req waiReq
        case parsed of
            Left _       -> next waiReq respond
            Right reqVal -> do
                resVal <- runner (handler (reqVal, waiReq))
                waiRes <- Resps.printResponses resContracts resVal
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

instance GServer n
    (S1 sm  (Rec0 (Canopy (Signature method path query headers body responses))))
    (S1 sm' (Rec0 (Server n (Signature method path query headers body responses)))) where
    gServe runner (M1 (K1 ep)) (M1 (K1 sv)) = tryServe runner ep sv

app ::
    forall server n.
    ( Generic (server Canopy)
    , Generic (server (Server n))
    , GServer n (Rep (server Canopy)) (Rep (server (Server n)))
    ) =>
    server Canopy ->
    (n ~> IO) ->
    server (Server n) ->
    Wai.Application
app endpoints runner handlers =
    gServe @n @(Rep (server Canopy)) @(Rep (server (Server n)))
        runner
        (from endpoints)
        (from handlers)
    $ \_ respond -> respond (Wai.responseLBS HTTP.status500 [] mempty)
