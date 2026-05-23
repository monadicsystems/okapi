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
import Okapi.Kind qualified as Kind
import Okapi.Mode (Endpoint (..), fn)
import Okapi.Req qualified as Req
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query qualified as Query
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Headers qualified as ResHeaders
import Okapi.Res.Status (KnownStatus)
import Okapi.ResAlt (GenericResAlt (..), resCase)

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

getUserReq
    = Req.get
    & Req.path do
        _      <- Path.lit "users"
        userId <- Path.param @Text
        pure userId
    & Req.query do
        nameFilter <- fst =. Query.optional (Query.param @Text "filter")
        limit      <- snd =. Query.optional (Query.param @Int  "limit")
        pure (nameFilter, limit)

-- ---------------------------------------------------------------------------
-- Responses
-- ---------------------------------------------------------------------------

type OkHeaders = (Text, Text)

okWithHeaders
    = Res.ok
    & Res.headers do
        ct  <- fst =. ResHeaders.param "content-type"
        loc <- snd =. ResHeaders.param "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    = Res.notFound
    & Res.headers (ResHeaders.param @RetryAfter "retry-after")

serverErrorPlain = Res.serverError

-- ---------------------------------------------------------------------------
-- Response sum type
-- ---------------------------------------------------------------------------

data GetUserRes f
    = OkRes       (Res f (KnownStatus Kind.S200) OkHeaders LBS.ByteString)
    | NotFoundRes (Res f (KnownStatus Kind.S404) RetryAfter LBS.ByteString)
    | ErrorRes    (Res f (KnownStatus Kind.S500) HTTP.ResponseHeaders LBS.ByteString)
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
