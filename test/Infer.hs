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
import Okapi.Res.Status (KS200, KS404, KS500)
import Okapi.ResAlt (GenericResAlt (..), resCase)

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

getUserReq
    = Req.get
    & Req.path do
        _      <- Req.lit @Text "users"
        userId <- Req.seg @Text "userId"
        pure userId
    & Req.query do
        nameFilter <- fst =. Req.paramOpt @Text "filter"
        limit      <- snd =. Req.paramOpt @Int  "limit"
        pure (nameFilter, limit)

-- ---------------------------------------------------------------------------
-- Responses
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Response sum type
-- ---------------------------------------------------------------------------

data GetUserRes f
    = OkRes       (Res f KS200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f KS404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f KS500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

getUserResCodec = resCase @GetUserRes
    okWithHeaders
    notFoundWithRetry
    serverErrorPlain

-- ---------------------------------------------------------------------------
-- Endpoint + server
-- ---------------------------------------------------------------------------

getUserEndpoint = getUserReq :-> getUserResCodec

getUserServer = fn \(reqData, _waiReq) -> do
    print (snd $ reqData.query_.value :: Maybe Int)
    pure $ OkRes $ Res.value 200 ("blah", "foo") do
        putStrLn "Returning..."
        pure ""

-- ---------------------------------------------------------------------------

main :: IO ()
main = putStrLn "okapi inference compiled"
