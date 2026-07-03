{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Okapi

getUserReq
    :: Request
        IsoCodec
        GET
        Text
        (Maybe Text)
        HTTP.RequestHeaders
        LBS.ByteString
getUserReq
    = mGet
    & path do
        _ <- seg_ @Text "users"
        userId <- seg @Text "userId"
        pure userId
    & query (param' "filter")

type OkHeaders = (Text, Text)

okWithHeaders
    :: Response IsoCodec S200 OkHeaders LBS.ByteString
okWithHeaders
    = s200
    & headers do
        ct  <- fst =. header "content-type"
        loc <- snd =. header "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    :: Response IsoCodec S404 RetryAfter LBS.ByteString
notFoundWithRetry
    = s404
    & headers (header "retry-after")

serverErrorPlain
    :: Response IsoCodec S500 HTTP.ResponseHeaders LBS.ByteString
serverErrorPlain = s500

data GetUserRes f
    = OkRes       (Response f S200 OkHeaders LBS.ByteString)
    | NotFoundRes (Response f S404 RetryAfter LBS.ByteString)
    | ErrorRes    (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic)

instance Cases GetUserRes

getUserResCodec =
    cases @GetUserRes
        okWithHeaders
        notFoundWithRetry
        serverErrorPlain

getUserEndpoint
    :: Contract
        ( Signature
            GET
            Text
            (Maybe Text)
            HTTP.RequestHeaders
            LBS.ByteString
            GetUserRes
        )
getUserEndpoint = getUserReq :-> getUserResCodec

getUserServer
    :: Function
        IO
        ( Signature
            GET
            Text
            (Maybe Text)
            HTTP.RequestHeaders
            LBS.ByteString
            GetUserRes
        )
getUserServer = fn \(_req, _waiReq) -> undefined

main :: IO ()
main = putStrLn "okapi examples compiled"
