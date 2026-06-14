{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Client (fetch, ClientSettings (..)) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Codec (Value (..))
import Okapi.Mode (Contract, ParseError, Signature, parseResponse, printRequest)
import Okapi.Request (Request)

data ClientSettings = ClientSettings
    { manager :: HC.Manager
    , baseUrl :: String
    }

fetch ::
    HC.Manager ->
    String ->
    Contract (Signature m p q h b r) ->
    Request Value m p q h b ->
    IO (Either ParseError (r Value))
fetch mgr baseUrl endpoint reqVal = do
    waiReq <- printRequest endpoint reqVal
    hcReq  <- toHCRequest baseUrl waiReq
    hcRes  <- HC.httpLbs hcReq mgr
    pure (parseResponse endpoint (fromHCResponse hcRes))

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
