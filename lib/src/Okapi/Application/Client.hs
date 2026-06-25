{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Application.Client (
    ClientError (..),
    ClientSettings (..),
    Client (..),
    fetch,
    GClient,
    client,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.Codec (Value (..))
import Okapi.Protocol.Request (Request)
import Okapi.Protocol.Response (Cases)
import Okapi.Contract (Signature)
import Okapi.Contract (Contract, parseResponses, printRequest)

data ClientError = ClientError deriving (Eq, Show)

data ClientSettings = ClientSettings
    { manager :: HC.Manager
    , baseUrl :: String
    }

data Client sig where
    Cb ::
        (Request Value method path query headers body -> IO (Either ClientError (responses Value))) ->
        Client (Signature method path query headers body responses)

fetch ::
    Cases responses =>
    HC.Manager ->
    String ->
    Contract (Signature method path query headers body responses) ->
    Request Value method path query headers body ->
    IO (Either ClientError (responses Value))
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

class GClient (epF :: Type -> Type) (clF :: Type -> Type) where
    gClient :: ClientSettings -> epF () -> clF ()

instance GClient epF clF => GClient (D1 dm epF) (D1 dm' clF) where
    gClient s (M1 ep) = M1 (gClient @epF @clF s ep)

instance GClient epF clF => GClient (C1 cm epF) (C1 cm' clF) where
    gClient s (M1 ep) = M1 (gClient @epF @clF s ep)

instance (GClient epL clL, GClient epR clR)
    => GClient (epL :*: epR) (clL :*: clR) where
    gClient s (epL :*: epR) =
        gClient @epL @clL s epL :*: gClient @epR @clR s epR

instance Cases responses => GClient
    (S1 sm  (Rec0 (Contract (Signature method path query headers body responses))))
    (S1 sm' (Rec0 (Client (Signature method path query headers body responses)))) where
    gClient (ClientSettings mgr url) (M1 (K1 ep)) =
        M1 (K1 (Cb \reqVal -> fetch mgr url ep reqVal))

client ::
    forall server.
    ( Generic (server Contract)
    , Generic (server Client)
    , GClient (Rep (server Contract)) (Rep (server Client))
    ) =>
    server Contract ->
    ClientSettings ->
    server Client
client endpoints settings =
    to (gClient @(Rep (server Contract)) @(Rep (server Client)) settings (from endpoints))
