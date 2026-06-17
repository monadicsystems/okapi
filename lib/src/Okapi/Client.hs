{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Client (ClientError (..), ClientSettings (..), fetch) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Codec (Value (..))
import Okapi.Mode (ClientError (..), Contract, Signature, parseResponses, printRequest)
import Okapi.Request (Request)
import Okapi.Responses (ResponseEnum)

data ClientSettings = ClientSettings
    { manager :: HC.Manager
    , baseUrl :: String
    }

fetch ::
    ResponseEnum r =>
    HC.Manager ->
    String ->
    Contract (Signature m p q h b r) ->
    Request Value m p q h b ->
    IO (Either ClientError (r Value))
fetch mgr baseUrl endpoint reqVal = do
    waiReq <- printRequest endpoint reqVal
    hcReq  <- toHCRequest baseUrl waiReq
    hcRes  <- HC.httpLbs hcReq mgr
    either (const (Left ClientError)) Right <$> parseResponses endpoint (fromHCResponse hcRes)

toHCRequest :: String -> Wai.Request -> IO HC.Request
toHCRequest baseUrl waiReq = do
    body <- Wai.strictRequestBody waiReq
    base <- HC.parseUrlThrow baseUrl
    let pathBS = "/" <> BS.intercalate "/" (map encodeUtf8 (Wai.pathInfo waiReq))
        qs     = HTTP.renderQuery True (Wai.queryString waiReq)
    pure base
        { HC.method         = Wai.requestMethod  waiReq
        , HC.path           = pathBS
        , HC.queryString    = qs
        , HC.requestHeaders = Wai.requestHeaders waiReq
        , HC.requestBody    = HC.RequestBodyLBS  body
        , HC.checkResponse  = \_ _ -> pure ()
        }

fromHCResponse :: HC.Response LBS.ByteString -> Wai.Response
fromHCResponse hcRes = Wai.responseLBS
    (HC.responseStatus  hcRes)
    (HC.responseHeaders hcRes)
    (HC.responseBody    hcRes)
