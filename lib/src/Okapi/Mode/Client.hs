{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode.Client (
    ClientError (..),
    ClientSettings (..),
    Client,
    pattern Fn,
    fetch,
    clientFor,
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
import Okapi.Mode.Forest (Forest (..), Shape, stripTags)
import Okapi.Record.Data qualified as Data
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Responses qualified as Resps

data ClientError = ClientError deriving (Eq, Show)

data ClientSettings = ClientSettings
    { manager :: HC.Manager
    , baseUrl :: String
    }

data Client shape where
    Function ::
        (Data.Request method path query headers body -> IO (Either ClientError result)) ->
        Client (Shape method path query headers body result)

pattern Fn ::
    (Data.Request method path query headers body -> IO (Either ClientError result)) ->
    Client (Shape method path query headers body result)
pattern Fn f <- Function f

fetch ::
    HC.Manager ->
    String ->
    Forest (Shape method path query headers body result) ->
    Data.Request method path query headers body ->
    IO (Either ClientError result)
fetch mgr baseUrl endpoint reqVal = case stripTags endpoint of
    (req :-> singleRes) -> do
        waiReq <- Req.printer req reqVal
        hcReq  <- toHCRequest baseUrl waiReq
        hcRes  <- HC.httpLbs hcReq mgr
        either (const (Left ClientError)) Right <$>
            Res.parser singleRes (fromHCResponse hcRes)
    (req :-< resContracts) -> do
        waiReq <- Req.printer req reqVal
        hcReq  <- toHCRequest baseUrl waiReq
        hcRes  <- HC.httpLbs hcReq mgr
        either (const (Left ClientError)) Right <$>
            Resps.parseResponses resContracts (fromHCResponse hcRes)
    Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"

-- | Build a single callable client function from one endpoint contract —
--   the single-endpoint counterpart to 'client', which does exactly this
--   per field for a whole record of endpoints via 'GClient' (see that
--   instance's body: it's the same @Function \\reqVal -> fetch mgr url ep
--   reqVal@ shape, just generalized over a whole record instead of one
--   'Forest'). Unwrap the result with 'Fn' to get the plain function:
--
--   > case clientFor settings endpoint of Fn f -> f requestValue
clientFor ::
    ClientSettings ->
    Forest (Shape method path query headers body result) ->
    Client (Shape method path query headers body result)
clientFor (ClientSettings mgr url) endpoint = Function (fetch mgr url endpoint)

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

instance GClient
    (S1 sm  (Rec0 (Forest (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Client (Shape method path query headers body result)))) where
    gClient (ClientSettings mgr url) (M1 (K1 ep)) =
        M1 (K1 (Function \reqVal -> fetch mgr url ep reqVal))

client ::
    forall server.
    ( Generic (server Forest)
    , Generic (server Client)
    , GClient (Rep (server Forest)) (Rep (server Client))
    ) =>
    server Forest ->
    ClientSettings ->
    server Client
client endpoints settings =
    to (gClient @(Rep (server Forest)) @(Rep (server Client)) settings (from endpoints))
