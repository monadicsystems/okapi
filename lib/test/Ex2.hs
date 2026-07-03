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
import Okapi
import System.Exit (exitFailure)

-- ── Response types ───────────────────────────────────────────────────────────

data UserRes f
    = FoundUser (Response f S200 Text LBS.ByteString)
    | NotFound  (Response f S404 Text LBS.ByteString)
    deriving (Generic, Cases)

data HealthRes f = HealthRes (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, Cases)

-- ── Endpoints ────────────────────────────────────────────────────────────────

getUserReq
    = mGet
    & path do
        seg_ @Text "users"
        uid <- seg @Text "id"
        pure uid

getUserEndpoint
    = getUserReq :-> cases @UserRes
        (s200 & headers do header @Text "x-user-id")
        (s404 & headers do header @Text "x-error")

healthReq
    = mGet
    & path do
        seg_ @Text "health"

healthEndpoint = healthReq :-> cases @HealthRes s200

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
    HealthRes

data MyApi f = MyApi
    { userEp   :: f SigUser
    , healthEp :: f SigHealth
    } deriving (Generic)

-- ── Contract values ──────────────────────────────────────────────────────────

myEndpoints :: MyApi Contract
myEndpoints = MyApi
    { userEp   = getUserEndpoint
    , healthEp = healthEndpoint
    }

-- ── Handlers ─────────────────────────────────────────────────────────────────

myHandlers :: MyApi (Function IO)
myHandlers = MyApi
    { userEp = fn \(req, _) ->
        pure if req.path.value == ("alice" :: Text)
            then FoundUser (response S200 "alice" (pure "found alice"))
            else NotFound  (response S404 "not found" (pure ""))
    , healthEp = fn \_ ->
        pure (HealthRes (response S200 [] (pure "healthy")))
    }

-- ── Grouped application ───────────────────────────────────────────────────────

myApp = server myEndpoints id myHandlers

-- ── Helpers ──────────────────────────────────────────────────────────────────

check :: String -> Bool -> IO ()
check name True  = putStrLn ("PASS: " ++ name)
check name False = putStrLn ("FAIL: " ++ name) >> exitFailure

-- ── Main ─────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    let _ = openApi myEndpoints

    mgr <- HC.newManager HC.defaultManagerSettings
    Warp.testWithApplication (pure myApp) \port -> do
        let cs = ClientSettings{manager = mgr, baseUrl = "http://localhost:" ++ show port}
            MyApi (Interface goUser) (Interface goHealth) = client myEndpoints cs

        r1 <- goUser (request GET "alice" [] [] (pure ""))
        case r1 of
            Right (FoundUser r) -> do
                bodyVal <- r.body.value
                check "GET /users/alice: status"  (r.status.value == S200)
                check "GET /users/alice: body"    (bodyVal == ("found alice" :: LBS.ByteString))
            _ -> check "GET /users/alice" False

        r2 <- goUser (request GET "bob" [] [] (pure ""))
        case r2 of
            Right (NotFound r) ->
                check "GET /users/bob: status" (r.status.value == S404)
            _ -> check "GET /users/bob" False

        r3 <- goHealth (request GET () [] [] (pure ""))
        case r3 of
            Right (HealthRes r) -> do
                bodyVal <- r.body.value
                check "GET /health: status" (r.status.value == S200)
                check "GET /health: body"   (bodyVal == ("healthy" :: LBS.ByteString))
            _ -> check "GET /health" False

    putStrLn "all ex2 tests passed"
