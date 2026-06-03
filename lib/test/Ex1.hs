{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Client (call)
import Okapi.Codec ((=.), Value (..))
import Okapi.Mode (Endpoint (..), fn, serve)
import Okapi.Req qualified as Req
import Okapi.Req.Method qualified as Method
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Status (KnownStatus (..), S200, S404)
import Okapi.ResAlt (GenericResAlt (..), resCase)
import System.Exit (exitFailure)

check name True  = putStrLn ("PASS: " ++ name)
check name False = putStrLn ("FAIL: " ++ name) >> exitFailure

data UserRes f
    = FoundUser (Res f S200 (Text, Text) LBS.ByteString)
    | NoUser    (Res f S404 Text         LBS.ByteString)
    deriving (Generic, GenericResAlt)

userRequest
    = Req.get
    & Req.path do
        Req.lit @Text "users"
        uid <- Req.seg @Text "id"
        pure uid
    & Req.query do
        Req.param' @Text "format"
    & Req.headers do
        Req.cookie @Text "session"

foundUser
    = Res.ok
    & Res.headers do
        ct  <- fst =. Res.header @Text "content-type"
        loc <- snd =. Res.header @Text "location"
        pure (ct, loc)

noUser
    = Res.notFound
    & Res.headers do
        Res.header @Text "x-error"

userResponses = resCase @UserRes
    foundUser
    noUser

userEndpoint = userRequest :-> userResponses

userHandler = fn \(req, _) ->
    if req.path_.value == ("alice" :: Text)
    then pure $ FoundUser (Res.value S200 ("text/plain", "/users/alice") (pure "Hello, Alice!"))
    else pure $ NoUser    (Res.value S404 "user not found"               (pure ""))

userApp = serve id userEndpoint userHandler

aliceReq = Req.value Method.GET "alice" (Just "json") "tok" (pure mempty)
bobReq   = Req.value Method.GET "bob"   Nothing       "tok" (pure mempty)

main = do
    mgr <- HC.newManager HC.defaultManagerSettings
    Warp.testWithApplication (pure userApp) \port -> do
        let go = call mgr ("http://localhost:" ++ show port) userEndpoint

        found <- go aliceReq
        case found of
            Left e              -> putStrLn ("FAIL: " ++ show e) >> exitFailure
            Right (FoundUser r) -> do
                body <- r.body_.value
                check "FoundUser: status"  (r.status_.value  == S200)
                check "FoundUser: headers" (r.headers_.value == ("text/plain", "/users/alice"))
                check "FoundUser: body"    (body             == "Hello, Alice!")
            Right (NoUser _)    -> putStrLn "FAIL: expected FoundUser" >> exitFailure

        miss <- go bobReq
        case miss of
            Left e              -> putStrLn ("FAIL: " ++ show e) >> exitFailure
            Right (NoUser r)    -> do
                body <- r.body_.value
                check "NoUser: status"  (r.status_.value  == S404)
                check "NoUser: headers" (r.headers_.value == "user not found")
                check "NoUser: body"    (body             == "")
            Right (FoundUser _) -> putStrLn "FAIL: expected NoUser" >> exitFailure
