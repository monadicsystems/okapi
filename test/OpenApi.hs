{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function ((&))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec ((=.))
import Okapi.Mode (Endpoint (..))
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Req qualified as Req
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Status (KS200, KS404, KS500)
import Okapi.ResAlt (GenericResAlt (..), only)

-- ---------------------------------------------------------------------------
-- GET /users/:id

type OkHeaders = (Text, Text)
type RetryAfter = Int

data GetUserRes f
    = OkRes       (Res f KS200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f KS404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f KS500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

okRes = Res.ok & Res.headers do
    ct  <- fst =. Res.header "content-type"
    loc <- snd =. Res.header "location"
    pure (ct, loc)

notFoundRes = Res.notFound & Res.headers (Res.header @RetryAfter "retry-after")

errRes = Res.serverError

getUserReq = Req.get
    & Req.path do
        _ <- Req.lit @Text "users"
        userId <- Req.seg @Text "userId"
        pure userId
    & Req.query (Req.paramOpt @Text "filter")

getUserEndpoint = getUserReq :-> resCase @GetUserRes
    okRes
    notFoundRes
    errRes

-- ---------------------------------------------------------------------------
-- POST /users

data CreateUserBody = CreateUserBody
    { username :: Text
    , email    :: Text
    } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

createUserReq = Req.post
    & Req.path (Req.lit @Text "users")
    & Req.json @CreateUserBody

createUserEndpoint = createUserReq :-> only (Res.ok & Res.json @CreateUserBody)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    LBS8.putStrLn (Aeson.encode (endpointToOpenApi createUserEndpoint <> endpointToOpenApi getUserEndpoint))
    putStrLn "okapi openapi compiled"
