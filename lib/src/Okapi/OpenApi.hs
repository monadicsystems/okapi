{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.OpenApi (endpointToOpenApi) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (?~))
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi, Operation, Param, ParamLocation (..), PathItem (..), Referenced (..), Response, Responses (..), toSchema)
import Data.OpenApi qualified as OA
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, typeRep)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), IsoCodec (..))
import Okapi.Mode (Endpoint (..), Signature)
import Okapi.Req (body_, headers_, method_, path_, query_)
import Okapi.Req.Body qualified as ReqBody
import Okapi.Req.Headers (Headers)
import Okapi.Req.Headers qualified as ReqH
import Okapi.Req.Method qualified as Method
import Okapi.Req.Path (Path)
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query (Query)
import Okapi.Req.Query qualified as Query
import Okapi.Res (status_)
import Okapi.Res qualified as ORes
import Okapi.Res.Body qualified as ResBody
import Okapi.Res.Headers qualified as ResH
import Okapi.Res.Status qualified as Status
import Okapi.ResAlt (ResAlt (..))
import Okapi.Data (ToPathData (..))

-- ---------------------------------------------------------------------------
-- Path

data PathPiece = PLit Text | PParam Text OA.Schema

walkPath :: Codec Path i o -> [PathPiece] -> [PathPiece]
walkPath (Embed (Path.Lit x))     ps = ps ++ [PLit (toUrlPiece x)]
walkPath (Embed h@(Path.Seg n))   ps = ps ++ [PParam n (typeRepSchema (typeRep (proxyOf h)))]
walkPath (Embed Path.Segs)        ps = ps ++ [PParam "segs" (mempty & OA.type_ ?~ OA.OpenApiString)]
walkPath (Embed Path.Raw)         ps = ps
walkPath (FMap _ c)               ps = walkPath c ps
walkPath (LMap _ c)               ps = walkPath c ps
walkPath (Apply cf cx)            ps = walkPath cx (walkPath cf ps)
walkPath (Pure _)                 ps = ps

pathTemplate :: [PathPiece] -> FilePath
pathTemplate pieces = "/" <> intercalate "/" (map piece pieces)
  where
    piece (PLit t)     = T.unpack t
    piece (PParam n _) = "{" <> T.unpack n <> "}"

pathOAParams :: [PathPiece] -> [Param]
pathOAParams pieces =
    [ mkParamWithSchema name ParamPath True sc
    | PParam name sc <- pieces
    ]

-- ---------------------------------------------------------------------------
-- Query

extractQueryParams :: Codec Query i o -> [Param]
extractQueryParams (Embed (Query.Param key))    = [mkParam key ParamQuery True]
extractQueryParams (Embed (Query.ParamOpt key)) = [mkParam key ParamQuery False]
extractQueryParams (Embed (Query.Flag key))     = [mkParam key ParamQuery True]
extractQueryParams (Embed (Query.FlagOpt key))  = [mkParam key ParamQuery False]
extractQueryParams (Embed Query.Raw)             = []
extractQueryParams (FMap _ c)                    = extractQueryParams c
extractQueryParams (LMap _ c)                    = extractQueryParams c
extractQueryParams (Apply cf cx)                 = extractQueryParams cf ++ extractQueryParams cx
extractQueryParams (Pure _)                      = []

-- ---------------------------------------------------------------------------
-- Header schema — Typeable-based dispatch for scalar header value types

proxyOf :: f a -> Proxy a
proxyOf _ = Proxy

innerProxyOf :: f (Maybe a) -> Proxy a
innerProxyOf _ = Proxy

typeRepSchema :: TypeRep -> OA.Schema
typeRepSchema tr
    | tr == typeRep (Proxy :: Proxy T.Text)  = mempty & OA.type_ ?~ OA.OpenApiString
    | tr == typeRep (Proxy :: Proxy Int)     = mempty & OA.type_ ?~ OA.OpenApiInteger
    | tr == typeRep (Proxy :: Proxy Integer) = mempty & OA.type_ ?~ OA.OpenApiInteger
    | tr == typeRep (Proxy :: Proxy Bool)    = mempty & OA.type_ ?~ OA.OpenApiBoolean
    | otherwise                              = mempty & OA.type_ ?~ OA.OpenApiString

-- ---------------------------------------------------------------------------
-- Request headers

extractHeaderParams :: Codec Headers i o -> [Param]
extractHeaderParams (Embed hdr) = case hdr of
    h@(ReqH.Header    key) -> [mkParamWithSchema (hdrName key) ParamHeader True  (typeRepSchema (typeRep (proxyOf h)))]
    h@(ReqH.HeaderOpt key) -> [mkParamWithSchema (hdrName key) ParamHeader False (typeRepSchema (typeRep (innerProxyOf h)))]
    ReqH.Raw               -> []
extractHeaderParams (FMap _ c)    = extractHeaderParams c
extractHeaderParams (LMap _ c)    = extractHeaderParams c
extractHeaderParams (Apply cf cx) = extractHeaderParams cf ++ extractHeaderParams cx
extractHeaderParams (Pure _)      = []

-- ---------------------------------------------------------------------------
-- Response headers

extractResHeaders :: Codec ResH.Headers i o -> [(Text, Bool, OA.Schema)]
extractResHeaders (Embed hdr) = case hdr of
    h@(ResH.Header    key) -> [(hdrName key, True,  typeRepSchema (typeRep (proxyOf h)))]
    h@(ResH.HeaderOpt key) -> [(hdrName key, False, typeRepSchema (typeRep (innerProxyOf h)))]
    ResH.Raw               -> []
extractResHeaders (FMap _ c)    = extractResHeaders c
extractResHeaders (LMap _ c)    = extractResHeaders c
extractResHeaders (Apply cf cx) = extractResHeaders cf ++ extractResHeaders cx
extractResHeaders (Pure _)      = []

-- ---------------------------------------------------------------------------
-- Body schema extraction — evidence lives in the Json GADT constructor
--
-- Helpers pass the GADT-refined Body value so the existential type is visible.

reqBodySchemaOf :: forall a. ReqBody.IsoJson a => ReqBody.Body (IO a) -> OA.Schema
reqBodySchemaOf _ = toSchema (Proxy @a)

resBodySchemaOf :: forall a. ResBody.IsoJson a => ResBody.Body (IO a) -> OA.Schema
resBodySchemaOf _ = toSchema (Proxy @a)

extractReqBodySchema :: Codec ReqBody.Body i o -> Maybe OA.Schema
extractReqBodySchema (Embed body)   = case body of
    ReqBody.Json -> Just (reqBodySchemaOf body)
    _            -> Nothing
extractReqBodySchema (FMap _ c)    = extractReqBodySchema c
extractReqBodySchema (LMap _ c)    = extractReqBodySchema c
extractReqBodySchema (Apply cf cx) = extractReqBodySchema cf <|> extractReqBodySchema cx
extractReqBodySchema (Pure _)      = Nothing

extractResBodySchema :: Codec ResBody.Body i o -> Maybe OA.Schema
extractResBodySchema (Embed body)   = case body of
    ResBody.Json -> Just (resBodySchemaOf body)
    _            -> Nothing
extractResBodySchema (FMap _ c)    = extractResBodySchema c
extractResBodySchema (LMap _ c)    = extractResBodySchema c
extractResBodySchema (Apply cf cx) = extractResBodySchema cf <|> extractResBodySchema cx
extractResBodySchema (Pure _)      = Nothing

-- ---------------------------------------------------------------------------
-- ResAlt walker

data ResInfo = ResInfo
    { resStatus     :: HTTP.Status
    , resBodySchema :: Maybe OA.Schema
    , resHdrNames   :: [(Text, Bool, OA.Schema)]
    }

extractResInfos :: ResAlt a -> [ResInfo]
extractResInfos (OneResAlt res) =
    [ ResInfo
        { resStatus     = fromMaybe HTTP.status200 (Status.extractStatus (isoCodec (status_ res)))
        , resBodySchema = extractResBodySchema (isoCodec (ORes.body_ res))
        , resHdrNames   = extractResHeaders (isoCodec (ORes.headers_ res))
        }
    ]
extractResInfos (ChoiceResAlt l r) = extractResInfos l ++ extractResInfos r

walkResAltCodec :: Codec ResAlt i o -> [ResInfo]
walkResAltCodec (Embed ra)    = extractResInfos ra
walkResAltCodec (FMap _ c)    = walkResAltCodec c
walkResAltCodec (LMap _ c)    = walkResAltCodec c
walkResAltCodec (Apply cf cx) = walkResAltCodec cf ++ walkResAltCodec cx
walkResAltCodec (Pure _)      = []

-- ---------------------------------------------------------------------------
-- Helpers

hdrName :: HTTP.HeaderName -> Text
hdrName = T.pack . BS8.unpack . CI.original

mkParam :: Text -> ParamLocation -> Bool -> Param
mkParam n loc req_ = mempty
    & OA.name     .~ n
    & OA.in_      .~ loc
    & OA.required ?~ req_

mkParamWithSchema :: Text -> ParamLocation -> Bool -> OA.Schema -> Param
mkParamWithSchema n loc req_ sc = mempty
    & OA.name     .~ n
    & OA.in_      .~ loc
    & OA.required ?~ req_
    & OA.schema   ?~ Inline sc

mkResResponse :: ResInfo -> Response
mkResResponse ri =
    mempty
    & OA.description .~ T.pack (show (HTTP.statusCode (resStatus ri)))
    & applyResBodySchema (resBodySchema ri)
    & applyResHeaders (resHdrNames ri)

applyResBodySchema :: Maybe OA.Schema -> Response -> Response
applyResBodySchema Nothing   r = r
applyResBodySchema (Just sc) r = r
    & OA.content .~ IHM.singleton "application/json"
        (mempty & OA.schema ?~ Inline sc)

applyResHeaders :: [(Text, Bool, OA.Schema)] -> Response -> Response
applyResHeaders [] r = r
applyResHeaders hs r = r
    & OA.headers .~ IHM.fromList
        [ (name, Inline (mempty & OA.required ?~ req_ & OA.schema ?~ Inline sc))
        | (name, req_, sc) <- hs
        ]

applyReqBodySchema :: Maybe OA.Schema -> Operation -> Operation
applyReqBodySchema Nothing   op = op
applyReqBodySchema (Just sc) op = op
    & OA.requestBody ?~ Inline
        ( mempty & OA.content .~ IHM.singleton "application/json"
            (mempty & OA.schema ?~ Inline sc)
        )

setMethod :: HTTP.StdMethod -> Operation -> PathItem -> PathItem
setMethod HTTP.GET    op pi_ = pi_ { _pathItemGet    = Just op }
setMethod HTTP.POST   op pi_ = pi_ { _pathItemPost   = Just op }
setMethod HTTP.PUT    op pi_ = pi_ { _pathItemPut    = Just op }
setMethod HTTP.DELETE op pi_ = pi_ { _pathItemDelete = Just op }
setMethod _           op pi_ = pi_ { _pathItemGet    = Just op }

-- ---------------------------------------------------------------------------
-- Main

endpointToOpenApi :: Endpoint (Signature m p q h b r) -> OpenApi
endpointToOpenApi (req :-> IsoCodec resAlt) =
    let
        stdMeth  = fromMaybe HTTP.GET (Method.extractMethod (isoCodec (method_ req)))
        pieces   = walkPath (isoCodec (path_ req)) []
        qParams  = extractQueryParams (isoCodec (query_ req))
        hParams  = extractHeaderParams (isoCodec (headers_ req))
        resInfos = walkResAltCodec resAlt
        reqBody  = if stdMeth `notElem` [HTTP.GET, HTTP.HEAD]
                   then extractReqBodySchema (isoCodec (body_ req))
                   else Nothing
        op = mempty
            & OA.parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
            & OA.responses  .~ Responses Nothing
                (IHM.fromList
                    [ (HTTP.statusCode (resStatus ri), Inline (mkResResponse ri))
                    | ri <- resInfos
                    ])
            & applyReqBodySchema reqBody
    in mempty
        & OA.info . OA.title   .~ "API"
        & OA.info . OA.version .~ "0.1.0"
        & OA.paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)
