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
import Okapi.Req.Method (GET)
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Status (S200, S404, S500)
import Okapi.ResAlt (GenericResAlt (..), ResAlt, resCase)

getUserReq
    :: Req
        IsoCodec
        GET
        Text
        (Maybe Text)
        HTTP.RequestHeaders
        LBS.ByteString
getUserReq
    = Req.get
    & Req.path do
        _ <- Req.lit @Text "users"
        userId <- Req.seg @Text "userId"
        pure userId
    & Req.query (Req.param' "filter")

type OkHeaders = (Text, Text)

okWithHeaders
    :: Res IsoCodec S200 OkHeaders LBS.ByteString
okWithHeaders
    = Res.ok
    & Res.headers do
        ct  <- fst =. Res.header "content-type"
        loc <- snd =. Res.header "location"
        pure (ct, loc)

type RetryAfter = Int

notFoundWithRetry
    :: Res IsoCodec S404 RetryAfter LBS.ByteString
notFoundWithRetry
    = Res.notFound
    & Res.headers (Res.header "retry-after")

serverErrorPlain
    :: Res IsoCodec S500 HTTP.ResponseHeaders LBS.ByteString
serverErrorPlain = Res.serverError

data GetUserRes f
    = OkRes       (Res f S200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f S404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic)

instance GenericResAlt GetUserRes

getUserResCodec :: IsoCodec ResAlt (GetUserRes Value)
getUserResCodec =
    resCase @GetUserRes
        okWithHeaders
        notFoundWithRetry
        serverErrorPlain

getUserEndpoint
    :: Endpoint
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
    :: Server
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
