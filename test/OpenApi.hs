{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec ((=.))
import Okapi.Mode (Endpoint (..))
import Okapi.OpenApi (endpointToOpenApi)
import Okapi.Req qualified as Req
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query qualified as Query
import Okapi.Res (Res)
import Okapi.Res qualified as Res
import Okapi.Res.Headers qualified as ResHeaders
import Okapi.Res.Status (KS200, KS404, KS500)
import Okapi.ResAlt (GenericResAlt (..))

type OkHeaders = (Text, Text)
type RetryAfter = Int

data GetUserRes f
    = OkRes       (Res f KS200 OkHeaders LBS.ByteString)
    | NotFoundRes (Res f KS404 RetryAfter LBS.ByteString)
    | ErrorRes    (Res f KS500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResAlt)

okRes = Res.ok & Res.headers do
    ct  <- fst =. ResHeaders.param "content-type"
    loc <- snd =. ResHeaders.param "location"
    pure (ct, loc)

notFoundRes = Res.notFound & Res.headers (ResHeaders.param @RetryAfter "retry-after")

errRes = Res.serverError

getUserReq = Req.get
    & Req.path do
        _ <- Path.lit "users"
        userId <- Path.param @Text
        pure userId
    & Req.query (Query.optional (Query.param @Text "filter"))

getUserEndpoint = getUserReq :-> resCase @GetUserRes okRes notFoundRes errRes

main :: IO ()
main = do
    let spec = endpointToOpenApi getUserEndpoint
    LBS8.putStrLn (Aeson.encode spec)
    putStrLn "okapi openapi compiled"
