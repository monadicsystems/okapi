{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module Okapi.Group
    ( GServable
    , GClientable
    , GOpenApiable
    , app
    , client
    , openApi
    ) where

import Data.Kind (Type)
import Data.OpenApi (OpenApi)
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Client (ClientSettings (..), fetch)
import Okapi.Mode (Client (..), Contract, Server (..), Signature, tryServe, type (~>))
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Responses (ResponseEnum)


-- ── GServable ────────────────────────────────────────────────────────────────

class GServable (n :: Type -> Type) (epF :: Type -> Type) (svF :: Type -> Type) where
    gServe :: (n ~> IO) -> epF () -> svF () -> Wai.Middleware

instance GServable n epF svF => GServable n (D1 dm epF) (D1 dm' svF) where
    gServe runner (M1 ep) (M1 sv) = gServe @n @epF @svF runner ep sv

instance GServable n epF svF => GServable n (C1 cm epF) (C1 cm' svF) where
    gServe runner (M1 ep) (M1 sv) = gServe @n @epF @svF runner ep sv

instance (GServable n epL svL, GServable n epR svR)
    => GServable n (epL :*: epR) (svL :*: svR) where
    gServe runner (epL :*: epR) (svL :*: svR) =
        gServe @n @epL @svL runner epL svL
        . gServe @n @epR @svR runner epR svR

instance ResponseEnum r => GServable n
    (S1 sm  (Rec0 (Contract (Signature m p q h b r))))
    (S1 sm' (Rec0 (Server n (Signature m p q h b r)))) where
    gServe runner (M1 (K1 ep)) (M1 (K1 sv)) = tryServe runner ep sv


-- ── app ──────────────────────────────────────────────────────────────────────

-- | Derive a WAI application from a record of contracts and a matching record of handlers.
app ::
    forall server n.
    ( Generic (server Contract)
    , Generic (server (Server n))
    , GServable n (Rep (server Contract)) (Rep (server (Server n)))
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


-- ── GClientable ──────────────────────────────────────────────────────────────

class GClientable (epF :: Type -> Type) (clF :: Type -> Type) where
    gClient :: ClientSettings -> epF () -> clF ()

instance GClientable epF clF => GClientable (D1 dm epF) (D1 dm' clF) where
    gClient s (M1 ep) = M1 (gClient @epF @clF s ep)

instance GClientable epF clF => GClientable (C1 cm epF) (C1 cm' clF) where
    gClient s (M1 ep) = M1 (gClient @epF @clF s ep)

instance (GClientable epL clL, GClientable epR clR)
    => GClientable (epL :*: epR) (clL :*: clR) where
    gClient s (epL :*: epR) =
        gClient @epL @clL s epL :*: gClient @epR @clR s epR

instance ResponseEnum r => GClientable
    (S1 sm  (Rec0 (Contract (Signature m p q h b r))))
    (S1 sm' (Rec0 (Client (Signature m p q h b r)))) where
    gClient (ClientSettings mgr url) (M1 (K1 ep)) =
        M1 (K1 (Cb \reqVal -> fetch mgr url ep reqVal))


-- ── client ───────────────────────────────────────────────────────────────────

-- | Derive a record of HTTP client functions from a record of contracts.
client ::
    forall server.
    ( Generic (server Contract)
    , Generic (server Client)
    , GClientable (Rep (server Contract)) (Rep (server Client))
    ) =>
    server Contract ->
    ClientSettings ->
    server Client
client endpoints settings =
    to (gClient @(Rep (server Contract)) @(Rep (server Client)) settings (from endpoints))


-- ── GOpenApiable ─────────────────────────────────────────────────────────────

class GOpenApiable (epF :: Type -> Type) where
    gOpenApi :: epF () -> OpenApi

instance GOpenApiable epF => GOpenApiable (D1 dm epF) where
    gOpenApi (M1 ep) = gOpenApi @epF ep

instance GOpenApiable epF => GOpenApiable (C1 cm epF) where
    gOpenApi (M1 ep) = gOpenApi @epF ep

instance (GOpenApiable epL, GOpenApiable epR) => GOpenApiable (epL :*: epR) where
    gOpenApi (epL :*: epR) = gOpenApi @epL epL <> gOpenApi @epR epR

instance GOpenApiable (S1 sm (Rec0 (Contract (Signature m p q h b r)))) where
    gOpenApi (M1 (K1 ep)) = endpointToOpenApi ep


-- ── openApi ──────────────────────────────────────────────────────────────────

-- | Derive an OpenAPI 3.0 document from a record of contracts.
openApi ::
    forall server.
    ( Generic (server Contract)
    , GOpenApiable (Rep (server Contract))
    ) =>
    server Contract ->
    OpenApi
openApi = gOpenApi @(Rep (server Contract)) . from
