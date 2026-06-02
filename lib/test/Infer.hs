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
import Okapi.Mode (Endpoint (..), fn)
import Okapi.Req qualified as Req
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Status (S200, S404, S500)
import Okapi.ResAlt (GenericResAlt (..), resCase)

getUserReq
    = Req.get
    & Req.path do
        _      <- Req.lit @Text "users"
        userId <- Req.seg @Int "userId"
        pure userId
    & Req.query do
        nameFilter <- fst =. Req.param' @Text "filter"
        limit      <- snd =. Req.param' @Int  "limit"
        pure (nameFilter, limit)

type OkHeaders = (Text, Text)

okWithHeaders
    = Res.ok
    & Res.headers do
        ct  <- fst =. Res.header "content-type"
        loc <- snd =. Res.header "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    = Res.notFound
    & Res.headers (Res.header @RetryAfter "retry-after")

serverErrorPlain = Res.serverError

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
    = Req.get
    & Req.headers do
        sess   <- fst =. Req.cookie @Text "session"
        userId <- snd =. Req.cookie @Text "userId"
        pure (sess, userId)

cookieReqOptional
    = Req.get
    & Req.headers (Req.cookie' @Text "theme")

cookieRes
    = Res.ok
    & Res.headers (Res.setCookie @Text "session")

cookieResOptional
    = Res.ok
    & Res.headers (Res.setCookie' @Text "theme")

main :: IO ()
main = putStrLn "okapi inference compiled"
