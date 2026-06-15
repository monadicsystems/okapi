{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Okapi

getUserReq
    = mGet
    & path do
        _      <- seg_ @Text "users"
        userId <- seg @Int "userId"
        pure userId
    & query do
        nameFilter <- fst =. param' @Text "filter"
        limit      <- snd =. param' @Int  "limit"
        pure (nameFilter, limit)

type OkHeaders = (Text, Text)

okWithHeaders
    = s200
    & headers do
        ct  <- fst =. header "content-type"
        loc <- snd =. header "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    = s404
    & headers (header @RetryAfter "retry-after")

serverErrorPlain = s500

data GetUserRes f
    = OkRes       (Response f S200 OkHeaders LBS.ByteString)
    | NotFoundRes (Response f S404 RetryAfter LBS.ByteString)
    | ErrorRes    (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, ResponseEnum)

getUserResCodec = responsesOf @GetUserRes
    okWithHeaders
    notFoundWithRetry
    serverErrorPlain

getUserEndpoint = getUserReq :-> getUserResCodec

getUserServer = fn \(reqData, _waiReq) -> do
    print (snd $ value reqData.query :: Maybe Int)
    pure $ OkRes $ response 200 ("blah", "foo") do
        putStrLn "Returning..."
        pure ""

cookieReq
    = mGet
    & headers do
        sess   <- fst =. cookie @Text "session"
        userId <- snd =. cookie @Text "userId"
        pure (sess, userId)

cookieReqOptional
    = mGet
    & headers (cookie' @Text "theme")

cookieRes
    = s200
    & headers (setCookie @Text "session")

cookieResOptional
    = s200
    & headers (setCookie' @Text "theme")

main :: IO ()
main = putStrLn "okapi inference compiled"
