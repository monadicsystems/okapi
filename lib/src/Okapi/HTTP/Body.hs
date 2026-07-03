module Okapi.HTTP.Body (
    IsoJson,
    jsonDecode,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.OpenApi (ToSchema)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

jsonDecode :: IsoJson a => LBS.ByteString -> IO a
jsonDecode bs = case Aeson.eitherDecode bs of
    Left err -> fail err
    Right x  -> pure x
