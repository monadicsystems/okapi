{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Artifact.Client (
    ClientError (..),
    ClientSettings (..),
    Client,
    pattern Fn,
    fetch,
    clientFor,
    GClient,
    client,
    GClientVia,
    clientVia,
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
import Network.HTTP.Types qualified as Types
import Network.Wai qualified as Wai
import Okapi.HTTP (HTTP (..), Shape, stripTags, Morph (..))
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Responses qualified as Resps
import Okapi.Data.Request qualified as Data

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
    HTTP (Shape method path query headers body result) ->
    Data.Request method path query headers body ->
    IO (Either ClientError result)
fetch mgr baseUrl contract reqVal = case stripTags contract of
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

-- | Build a single callable client function from one contract —
--   the single-contract counterpart to 'client', which does exactly this
--   per field for a whole record of contracts via 'GClient' (see that
--   instance's body: it's the same @Function \\reqVal -> fetch mgr url ct
--   reqVal@ shape, just generalized over a whole record instead of one
--   'HTTP'). Unwrap the result with 'Fn' to get the plain function:
--
--   > case clientFor settings contract of Fn f -> f requestValue
clientFor ::
    ClientSettings ->
    HTTP (Shape method path query headers body result) ->
    Client (Shape method path query headers body result)
clientFor (ClientSettings mgr url) contract = Function (fetch mgr url contract)

toHCRequest :: String -> Wai.Request -> IO HC.Request
toHCRequest baseUrl waiReq = do
    body <- Wai.strictRequestBody waiReq
    base <- HC.parseUrlThrow baseUrl
    let pathBS = "/" <> BS.intercalate "/" (map encodeUtf8 (Wai.pathInfo waiReq))
        qs     = Types.renderQuery True (Wai.queryString waiReq)
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

class GClient (ctF :: Type -> Type) (clF :: Type -> Type) where
    gClient :: ClientSettings -> ctF () -> clF ()

instance GClient ctF clF => GClient (D1 dm ctF) (D1 dm' clF) where
    gClient s (M1 ct) = M1 (gClient @ctF @clF s ct)

instance GClient ctF clF => GClient (C1 cm ctF) (C1 cm' clF) where
    gClient s (M1 ct) = M1 (gClient @ctF @clF s ct)

instance (GClient ctL clL, GClient ctR clR)
    => GClient (ctL :*: ctR) (clL :*: clR) where
    gClient s (ctL :*: ctR) =
        gClient @ctL @clL s ctL :*: gClient @ctR @clR s ctR

instance GClient
    (S1 sm  (Rec0 (HTTP (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Client (Shape method path query headers body result)))) where
    gClient (ClientSettings mgr url) (M1 (K1 ct)) =
        M1 (K1 (Function \reqVal -> fetch mgr url ct reqVal))

-- | Lets a field be a nested record of the same shape instead of a
--   concrete 'HTTP'\/'Client' — recurses via 'client' itself. Same
--   non-overlap argument as the nested instances in "Okapi.Artifact.Endpoint".
instance
    ( Generic (nested HTTP)
    , Generic (nested Client)
    , GClient (Rep (nested HTTP)) (Rep (nested Client))
    ) =>
    GClient (S1 sm (Rec0 (nested HTTP))) (S1 sm' (Rec0 (nested Client)))
    where
    gClient settings (M1 (K1 ctVal)) = M1 (K1 (client ctVal settings))

client ::
    forall record.
    ( Generic (record HTTP)
    , Generic (record Client)
    , GClient (Rep (record HTTP)) (Rep (record Client))
    ) =>
    record HTTP ->
    ClientSettings ->
    record Client
client contracts settings =
    to (gClient @(Rep (record HTTP)) @(Rep (record Client)) settings (from contracts))

class GClientVia (ctF :: Type -> Type) (clF :: Type -> Type) where
    gClientVia :: ClientSettings -> ctF () -> clF ()

instance GClientVia ctF clF => GClientVia (D1 dm ctF) (D1 dm' clF) where
    gClientVia s (M1 ct) = M1 (gClientVia @ctF @clF s ct)

instance GClientVia ctF clF => GClientVia (C1 cm ctF) (C1 cm' clF) where
    gClientVia s (M1 ct) = M1 (gClientVia @ctF @clF s ct)

instance (GClientVia ctL clL, GClientVia ctR clR)
    => GClientVia (ctL :*: ctR) (clL :*: clR) where
    gClientVia s (ctL :*: ctR) =
        gClientVia @ctL @clL s ctL :*: gClientVia @ctR @clR s ctR

instance GClientVia
    (S1 sm  (Rec0 (Morph HTTP n (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Morph Client n (Shape method path query headers body result)))) where
    gClientVia (ClientSettings mgr url) (M1 (K1 (Morph ct))) =
        M1 (K1 (Morph (Function \reqVal -> fetch mgr url ct reqVal)))

-- | Lets a field be a nested record of the same shape instead of a
--   concrete @Morph HTTP@\/@Morph Client@ pair — recurses via
--   'clientVia' itself.
instance
    ( Generic (nested (Morph HTTP))
    , Generic (nested (Morph Client))
    , GClientVia (Rep (nested (Morph HTTP))) (Rep (nested (Morph Client)))
    ) =>
    GClientVia (S1 sm (Rec0 (nested (Morph HTTP)))) (S1 sm' (Rec0 (nested (Morph Client))))
    where
    gClientVia settings (M1 (K1 ctVal)) = M1 (K1 (clientVia ctVal settings))

{- | Heterogeneous-@n@ counterpart to 'client' — takes a record built with
  'Okapi.HTTP.Morph' (see 'Okapi.Artifact.Endpoint.endpointsVia') instead
  of a plain @record HTTP@. Output is @record (Morph Client)@, not
  plain @record Client@ — same reasoning as 'Okapi.Artifact.Link.linksVia':
  each field's @n@ is baked into the record's own field declarations, so
  the output has to stay 2-arg-shaped to match, even though 'Client' itself
  no more cares about @n@ than 'Link' does.
-}
clientVia ::
    forall record.
    ( Generic (record (Morph HTTP))
    , Generic (record (Morph Client))
    , GClientVia (Rep (record (Morph HTTP))) (Rep (record (Morph Client)))
    ) =>
    record (Morph HTTP) ->
    ClientSettings ->
    record (Morph Client)
clientVia contracts settings =
    to (gClientVia @(Rep (record (Morph HTTP))) @(Rep (record (Morph Client))) settings (from contracts))
