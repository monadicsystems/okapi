{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-missing-signatures #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec ((=.))
import Okapi.Kind qualified as Kind
import Okapi.Mode (Endpoint (..), Server, fn)
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
        _ <- Path.lit "users"
        userId <- Path.param @Text
        pure userId
    & Req.query do
        nameFilter <- fst =. Query.optional (Query.param @Text "filter")
        limit  <- snd =. Query.optional (Query.param @Int  "limit")
        pure (nameFilter, limit)

-- ---------------------------------------------------------------------------
-- Responses
-- ---------------------------------------------------------------------------

type OkHeaders = (Text, Text)

okWithHeaders
    = Res.ok
    & Res.resHeaders do
        ct  <- fst =. ResHeaders.param "content-type"
        loc <- snd =. ResHeaders.param "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    = Res.notFound
    & Res.resHeaders (ResHeaders.param @RetryAfter "retry-after")

serverErrorPlain = Res.serverError

-- ---------------------------------------------------------------------------
-- Response sum type
-- ---------------------------------------------------------------------------

data GetUserRes f
    = OkRes       (Res f (KnownStatus Kind.S200) OkHeaders LBS.ByteString)
    | NotFoundRes (Res f (KnownStatus Kind.S404) RetryAfter LBS.ByteString)
    | ErrorRes    (Res f (KnownStatus Kind.S500) HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic)

instance GenericResAlt GetUserRes

getUserResCodec
    = resCase @GetUserRes
        okWithHeaders
        notFoundWithRetry
        serverErrorPlain

-- ---------------------------------------------------------------------------
-- Endpoint + server
-- ---------------------------------------------------------------------------

getUserEndpoint = getUserReq :-> getUserResCodec

-- Minimal partial signature: only the monad IO is given; the rest is inferred.
getUserServer :: Server IO _
getUserServer = fn \(_req, _waiReq) ->
    pure $ OkRes $ Res.value 200 ("blah", "foo") (pure "")

-- ---------------------------------------------------------------------------

main :: IO ()
main = putStrLn "okapi inference compiled"
