{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function ((&))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Client (call)
import Okapi.Codec ((=.), Value (..))
import Okapi.Mode (Endpoint (..), fn, serve)
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Req qualified as Req
import Okapi.Req.Method qualified as Method
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Status (KnownStatus (..), S200, S404)
import Okapi.ResAlt (GenericResAlt (..), resCase)
import System.Exit (exitFailure)

data Address = Address
    { street :: Text
    , city   :: Text
    , zip    :: Text
    } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

data UserReqBody = UserReqBody
    { name    :: Text
    , email   :: Text
    , age     :: Int
    , address :: Address
    } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

data UserResBody = UserResBody
    { userId :: Text
    , name   :: Text
    , email  :: Text
    , age    :: Int
    , active :: Bool
    } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

data UserRes f
    = FoundUser (Res f S200 (Text, Text) UserResBody)
    | NoUser (Res f S404 Text LBS.ByteString)
    deriving (Generic, GenericResAlt)

userRequest
    = Req.post
    & Req.path do
        Req.lit @Text "users"
        uid <- Req.seg @Text "id"
        pure uid
    & Req.query do
        Req.param' @Text "format"
    & Req.headers do
        Req.cookie @Text "session"
    & Req.json @UserReqBody

foundUserHeaders = do
    ct  <- fst =. Res.header @Text "content-type"
    loc <- snd =. Res.header @Text "location"
    pure (ct, loc)

foundUser
    = Res.ok
    & Res.headers foundUserHeaders
    & Res.json @UserResBody

noUser
    = Res.notFound
    & Res.headers do
        Res.header @Text "x-error"

userResponses = resCase @UserRes
    foundUser
    noUser

userEndpoint = userRequest :-> userResponses

userHandler = fn \(req, _) -> do
    reqBody <- req.body_.value
    pure
        if req.path_.value == ("alice" :: Text)
        then
            let
                userId = req.path_.value
                name   = reqBody.name
                email  = reqBody.email
                age    = reqBody.age
                active = True
            in 
                FoundUser $ Res.value S200 ("application/json", "/users/alice") (pure UserResBody{..})
        else NoUser $ Res.value S404 "user not found" (pure "")

userApp = serve id userEndpoint userHandler

printSchema = LBS8.putStrLn (Pretty.encodePretty (endpointToOpenApi userEndpoint))

aliceReq = Req.value Method.POST "alice" (Just "json") "tok" $ pure
    UserReqBody
        { name    = "Alice"
        , email   = "alice@example.com"
        , age     = 30
        , address = Address "123 Main St" "Wonderland" "12345"
        }

bobReq = Req.value Method.POST "bob" Nothing "tok" $ pure
    UserReqBody
        { name    = "Bob"
        , email   = "bob@example.com"
        , age     = 25
        , address = Address "456 Elm St" "Nowhere" "67890"
        }

main = do
    mgr <- HC.newManager HC.defaultManagerSettings
    Warp.testWithApplication (pure userApp) \port -> do
        let go = call mgr ("http://localhost:" ++ show port) userEndpoint

        found <- go aliceReq
        case found of
            Left e              -> putStrLn ("FAIL: " ++ show e) >> exitFailure
            Right (FoundUser r) -> do
                resBody <- r.body_.value
                check "FoundUser: status"  (r.status_.value  == S200)
                check "FoundUser: headers" (r.headers_.value == ("application/json", "/users/alice"))
                check "FoundUser: userId"  (resBody.userId   == "alice")
                check "FoundUser: name"    (resBody.name     == "Alice")
                check "FoundUser: email"   (resBody.email    == "alice@example.com")
                check "FoundUser: age"     (resBody.age      == 30)
                check "FoundUser: active"  (resBody.active   == True)
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

check name True  = putStrLn ("PASS: " ++ name)
check name False = putStrLn ("FAIL: " ++ name) >> exitFailure
