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
import Okapi.Codec ((=.), value)
import Okapi.Mode (Contract (..), fn)
import Okapi.Request qualified as Req
import Okapi.Response (Res)
import Okapi.Response qualified as Res
import Okapi.Response.Status (S200, S404, S500)
import Okapi.Response.Alt (GenericResAlt (..), resCase)

getUserReq
    = Req.mGet
    & Req.path do
        _      <- Req.seg_ @Text "users"
        userId <- Req.seg @Int "userId"
        pure userId
    & Req.query do
        nameFilter <- fst =. Req.param' @Text "filter"
        limit      <- snd =. Req.param' @Int  "limit"
        pure (nameFilter, limit)

type OkHeaders = (Text, Text)

okWithHeaders
    = Res.s200
    & Res.headers do
        ct  <- fst =. Res.header "content-type"
        loc <- snd =. Res.header "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    = Res.s404
    & Res.headers (Res.header @RetryAfter "retry-after")

serverErrorPlain = Res.s500

data GetUserRes f
    = OkRes       (Res f S200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f S404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

getUserResCodec = resCase @GetUserRes
    okWithHeaders
    notFoundWithRetry
    serverErrorPlain

getUserEndpoint = getUserReq :-> getUserResCodec

getUserServer = fn \(reqData, _waiReq) -> do
    print (snd $ reqData.query_.value :: Maybe Int)
    pure $ OkRes $ Res.value 200 ("blah", "foo") do
        putStrLn "Returning..."
        pure ""

cookieReq
    = Req.mGet
    & Req.headers do
        sess   <- fst =. Req.cookie @Text "session"
        userId <- snd =. Req.cookie @Text "userId"
        pure (sess, userId)

cookieReqOptional
    = Req.mGet
    & Req.headers (Req.cookie' @Text "theme")

cookieRes
    = Res.s200
    & Res.headers (Res.setCookie @Text "session")

cookieResOptional
    = Res.s200
    & Res.headers (Res.setCookie' @Text "theme")

main :: IO ()
main = putStrLn "okapi inference compiled"
