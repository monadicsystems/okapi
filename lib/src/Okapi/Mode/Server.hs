{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode.Server (
    Server (..),
    fn,
    type (~>),
    serve,
    tryServe,
    GServer,
    server,
) where

import Data.Kind (Type)
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Mode.Forest (Forest (..), Shape)
import Okapi.Record.Data qualified as Data
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Responses qualified as Resps

data Server (n :: Type -> Type) shape where
    Function ::
        ((Data.Request method path query headers body, Wai.Request) -> n result) ->
        Server n (Shape method path query headers body result)

fn ::
    ((Data.Request method path query headers body, Wai.Request) -> n result) ->
    Server n (Shape method path query headers body result)
fn = Function

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

serve ::
    (n ~> IO) ->
    Forest (Shape method path query headers body result) ->
    Server n (Shape method path query headers body result) ->
    Wai.Application
serve runner endpoint (Function handler) waiReq respond = case endpoint of
    (req :-> singleRes) -> do
        parsed <- Req.parseRequest req waiReq
        case parsed of
            Left _       -> respond (Wai.responseLBS HTTP.status400 [] mempty)
            Right reqVal -> do
                resVal <- runner (handler (reqVal, waiReq))
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
    Forest (Shape method path query headers body result) ->
    Server n (Shape method path query headers body result) ->
    Wai.Middleware
tryServe runner endpoint (Function handler) next waiReq respond = case endpoint of
    (req :-> singleRes) -> do
        parsed <- Req.parseRequest req waiReq
        case parsed of
            Left _       -> next waiReq respond
            Right reqVal -> do
                resVal <- runner (handler (reqVal, waiReq))
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
    (S1 sm  (Rec0 (Forest (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Server n (Shape method path query headers body result)))) where
    gServe runner (M1 (K1 ep)) (M1 (K1 sv)) = tryServe runner ep sv

server ::
    forall server n.
    ( Generic (server Forest)
    , Generic (server (Server n))
    , GServer n (Rep (server Forest)) (Rep (server (Server n)))
    ) =>
    (n ~> IO) ->
    server Forest ->
    server (Server n) ->
    Wai.Application
server runner endpoints handlers =
    gServe @n @(Rep (server Forest)) @(Rep (server (Server n)))
        runner
        (from endpoints)
        (from handlers)
    $ \_ respond -> respond (Wai.responseLBS HTTP.status404 [] mempty)
