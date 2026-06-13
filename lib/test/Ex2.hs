{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Client (ClientSettings (..))
import Okapi.Codec (Value (..))
import Okapi.Group (app, client, openApi)
import Okapi.Mode (Client (..), Endpoint (..), Server (..), Signature, fn)
import Okapi.Req qualified as Req
import Okapi.Req.Method (GET)
import Okapi.Req.Method qualified as Method
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Status (KnownStatus (..), S200, S404)
import Okapi.ResAlt (GenericResAlt (..), Only (..), only, resCase)
import System.Exit (exitFailure)

-- ── Response types ───────────────────────────────────────────────────────────

data UserRes f
    = FoundUser (Res f S200 Text LBS.ByteString)
    | NotFound  (Res f S404 Text LBS.ByteString)
    deriving (Generic, GenericResAlt)

-- ── Endpoints ────────────────────────────────────────────────────────────────

getUserReq
    = Req.get
    & Req.path do
        Req.lit @Text "users"
        uid <- Req.seg @Text "id"
        pure uid

getUserEndpoint
    = getUserReq :-> resCase @UserRes
        (Res.ok    & Res.headers (Res.header @Text "x-user-id"))
        (Res.notFound & Res.headers (Res.header @Text "x-error"))

healthReq
    = Req.get
    & Req.path do
        Req.lit @Text "health"

healthEndpoint = healthReq :-> only Res.ok

-- ── HKD record ───────────────────────────────────────────────────────────────

type SigUser = Signature
    GET
    Text
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
    UserRes

type SigHealth = Signature
    GET
    ()
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
    (Only S200 HTTP.ResponseHeaders LBS.ByteString)

data MyApi f = MyApi
    { userEp   :: f SigUser
    , healthEp :: f SigHealth
    } deriving (Generic)

-- ── Endpoint values ──────────────────────────────────────────────────────────

myEndpoints :: MyApi Endpoint
myEndpoints = MyApi
    { userEp   = getUserEndpoint
    , healthEp = healthEndpoint
    }

-- ── Handlers ─────────────────────────────────────────────────────────────────

myHandlers :: MyApi (Server IO)
myHandlers = MyApi
    { userEp = fn \(req, _) ->
        pure if req.path_.value == ("alice" :: Text)
            then FoundUser (Res.value S200 "alice" (pure "found alice"))
            else NotFound  (Res.value S404 "not found" (pure ""))
    , healthEp = fn \_ ->
        pure (Only (Res.value S200 [] (pure "healthy")))
    }

-- ── Grouped application ───────────────────────────────────────────────────────

myApp = app myEndpoints id myHandlers

-- ── Helpers ──────────────────────────────────────────────────────────────────

check :: String -> Bool -> IO ()
check name True  = putStrLn ("PASS: " ++ name)
check name False = putStrLn ("FAIL: " ++ name) >> exitFailure

-- ── Main ─────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    let _ = openApi myEndpoints  -- smoke-test: generates OpenAPI for both endpoints

    mgr <- HC.newManager HC.defaultManagerSettings
    Warp.testWithApplication (pure myApp) \port -> do
        let cs = ClientSettings{manager = mgr, baseUrl = "http://localhost:" ++ show port}
            MyApi (Cb goUser) (Cb goHealth) = client myEndpoints cs

        -- user endpoint: known user
        r1 <- goUser (Req.value Method.GET "alice" [] [] (pure ""))
        case r1 of
            Right (FoundUser r) -> do
                body <- r.body_.value
                check "GET /users/alice: status"  (r.status_.value == S200)
                check "GET /users/alice: body"    (body == ("found alice" :: LBS.ByteString))
            _ -> check "GET /users/alice" False

        -- user endpoint: unknown user
        r2 <- goUser (Req.value Method.GET "bob" [] [] (pure ""))
        case r2 of
            Right (NotFound r) ->
                check "GET /users/bob: status" (r.status_.value == S404)
            _ -> check "GET /users/bob" False

        -- health endpoint
        r3 <- goHealth (Req.value Method.GET () [] [] (pure ""))
        case r3 of
            Right (Only r) -> do
                body <- r.body_.value
                check "GET /health: status" (r.status_.value == S200)
                check "GET /health: body"   (body == ("healthy" :: LBS.ByteString))
            _ -> check "GET /health" False

    putStrLn "all ex2 tests passed"
