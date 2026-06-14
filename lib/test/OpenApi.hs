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
import Okapi.Mode (Contract (..))
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Request qualified as Req
import Okapi.Response (Res)
import Okapi.Response qualified as Res
import Okapi.Response.Status (S200, S404, S500)
import Okapi.Response.Alt (GenericResAlt (..), only)

type OkHeaders = (Text, Text)
type RetryAfter = Int

data GetUserRes f
    = OkRes       (Res f S200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f S404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

okRes
    = Res.s200
    & Res.headers do
        ct  <- fst =. Res.header "content-type"
        loc <- snd =. Res.header "location"
        pure (ct, loc)

notFoundRes = Res.s404 & Res.headers (Res.header @RetryAfter "retry-after")

errRes = Res.s500

getUserReq
    = Req.mGet
    & Req.path do
        _ <- Req.seg_ @Text "users"
        userId <- Req.seg @Text "userId"
        pure userId
    & Req.query (Req.param' @Text "filter")

getUserEndpoint = getUserReq :-> resCase @GetUserRes
    okRes
    notFoundRes
    errRes

data CreateUserBody = CreateUserBody
    { username :: Text
    , email    :: Text
    } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

createUserReq = Req.mPost
    & Req.path (Req.seg_ @Text "users")
    & Req.json @CreateUserBody

createUserEndpoint = createUserReq :-> only (Res.s200 & Res.json @CreateUserBody)

main :: IO ()
main = do
    LBS8.putStrLn (Aeson.encode (endpointToOpenApi createUserEndpoint <> endpointToOpenApi getUserEndpoint))
    putStrLn "okapi openapi compiled"
