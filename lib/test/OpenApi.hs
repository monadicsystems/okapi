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
import Okapi

type OkHeaders = (Text, Text)
type RetryAfter = Int

data GetUserRes f
    = OkRes       (Response f S200 OkHeaders LBS.ByteString)
    | NotFoundRes (Response f S404 RetryAfter LBS.ByteString)
    | ErrorRes    (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, ResponseEnum)

data CreateUserRes f = CreateUserRes (Response f S200 HTTP.ResponseHeaders CreateUserBody)
    deriving (Generic, ResponseEnum)

okRes
    = s200
    & headers do
        ct  <- fst =. header "content-type"
        loc <- snd =. header "location"
        pure (ct, loc)

notFoundRes = s404 & headers (header @RetryAfter "retry-after")

errRes = s500

getUserReq
    = mGet
    & path do
        _ <- seg_ @Text "users"
        userId <- seg @Text "userId"
        pure userId
    & query (param' @Text "filter")

getUserEndpoint = getUserReq :-> responsesOf @GetUserRes
    okRes
    notFoundRes
    errRes

data CreateUserBody = CreateUserBody
    { username :: Text
    , email    :: Text
    } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON, ToSchema)

createUserReq = mPost
    & path (seg_ @Text "users")
    & body (json @CreateUserBody)

createUserEndpoint = createUserReq :-> responsesOf @CreateUserRes (s200 & body (json @CreateUserBody))

main :: IO ()
main = do
    LBS8.putStrLn (Aeson.encode (endpointToOpenApi createUserEndpoint <> endpointToOpenApi getUserEndpoint))
    putStrLn "okapi openapi compiled"
