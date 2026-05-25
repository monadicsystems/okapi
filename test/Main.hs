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
import Okapi.Codec (IsoCodec (..), Value, (=.))
import Okapi.Mode (Endpoint (..), Server, Signature, fn)
import Okapi.Req (Req)
import Okapi.Req qualified as Req
import Okapi.Req.Method (KGET)
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query qualified as Query
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Headers qualified as ResHeaders
import Okapi.Res.Status (KS200, KS404, KS500)
import Okapi.ResAlt (GenericResAlt (..), ResAlt, resCase)

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | GET /users/:id?filter=<text>
getUserReq
    :: Req
        IsoCodec
        KGET
        Text
        (Maybe Text)
        HTTP.RequestHeaders
        LBS.ByteString
getUserReq
    = Req.get
    & Req.path do
        _ <- Path.lit "users"
        userId <- Path.param @Text
        pure userId
    & Req.query (Query.optional (Query.param "filter"))

-- ---------------------------------------------------------------------------
-- Responses
-- ---------------------------------------------------------------------------

-- | 200 — typed content-type and location headers
type OkHeaders = (Text, Text)

okWithHeaders
    :: Res IsoCodec KS200 OkHeaders LBS.ByteString
okWithHeaders
    = Res.ok
    & Res.headers do
        ct  <- fst =. ResHeaders.param "content-type"
        loc <- snd =. ResHeaders.param "location"
        pure (ct, loc)

-- | 404 — one retry-after header
type RetryAfter = Int

notFoundWithRetry
    :: Res IsoCodec KS404 RetryAfter LBS.ByteString
notFoundWithRetry
    = Res.notFound
    & Res.headers (ResHeaders.param "retry-after")

-- | 500 — raw response headers, no specialisation
serverErrorPlain
    :: Res IsoCodec KS500 HTTP.ResponseHeaders LBS.ByteString
serverErrorPlain = Res.serverError

-- ---------------------------------------------------------------------------
-- Response sum type
-- ---------------------------------------------------------------------------

data GetUserRes f
    = OkRes       (Res f KS200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f KS404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f KS500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic)

instance GenericResAlt GetUserRes

getUserResCodec :: IsoCodec ResAlt (GetUserRes Value)
getUserResCodec =
    resCase @GetUserRes
        okWithHeaders
        notFoundWithRetry
        serverErrorPlain

-- ---------------------------------------------------------------------------
-- Endpoint + server
-- ---------------------------------------------------------------------------

getUserEndpoint
    :: Endpoint
        ( Signature
            KGET
            Text
            (Maybe Text)
            HTTP.RequestHeaders
            LBS.ByteString
            GetUserRes
        )
getUserEndpoint = getUserReq :-> getUserResCodec

getUserServer
    :: Server
        IO
        ( Signature
            KGET
            Text
            (Maybe Text)
            HTTP.RequestHeaders
            LBS.ByteString
            GetUserRes
        )
getUserServer = fn \(_req, _waiReq) -> undefined

-- ---------------------------------------------------------------------------

main :: IO ()
main = putStrLn "okapi examples compiled"
