{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.OpenApi (OpenApi, Operation, Param, ParamLocation (..), PathItem (..), Referenced (..), Response, declareSchemaRef, toSchema)
import Data.OpenApi qualified as OA
import Data.OpenApi.Declare (execDeclare)
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, typeRep)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP
import Okapi.Body (Body, IsoJson)
import Okapi.Body qualified as Body
import Okapi.Codec (Codec (..), IsoCodec (..))
import Okapi.Headers (ForRequest, ForResponse, Headers (..))
import Okapi.Mode (Contract (..), Signature)
import Okapi.Request (Request (..))

import Okapi.Request.Method qualified as Method
import Okapi.Request.Path (Path)
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query (Query)
import Okapi.Request.Query qualified as Query

import Okapi.Response qualified as ORes
import Okapi.Response.Status qualified as Status
import Okapi.Responses (Responses (..))
import Okapi.Data (ToPathData (..))

data PathPiece = PLit Text | PParam Text OA.Schema

walkPath :: Codec Path i o -> [PathPiece] -> [PathPiece]
walkPath (Embed (Path.Seg_ x))    ps = ps ++ [PLit (toUrlPiece x)]
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

extractQueryParams :: Codec Query i o -> [Param]
extractQueryParams (Embed (Query.Param key))    = [mkParam key ParamQuery True]
extractQueryParams (Embed (Query.Param' key)) = [mkParam key ParamQuery False]
extractQueryParams (Embed (Query.Flag key))   = [mkParam key ParamQuery True]
extractQueryParams (Embed (Query.Flag' key))  = [mkParam key ParamQuery False]
extractQueryParams (Embed Query.Raw)             = []
extractQueryParams (FMap _ c)                    = extractQueryParams c
extractQueryParams (LMap _ c)                    = extractQueryParams c
extractQueryParams (Apply cf cx)                 = extractQueryParams cf ++ extractQueryParams cx
extractQueryParams (Pure _)                      = []

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

extractHeaderParams :: Codec (Headers ForRequest) i o -> [Param]
extractHeaderParams (Embed hdr) = case hdr of
    h@(Header   key)  -> [mkParamWithSchema (hdrName key) ParamHeader True  (typeRepSchema (typeRep (proxyOf h)))]
    h@(Header'  key)  -> [mkParamWithSchema (hdrName key) ParamHeader False (typeRepSchema (typeRep (innerProxyOf h)))]
    h@(Cookie   name) -> [mkParamWithSchema (T.pack (BS8.unpack name)) ParamCookie True  (typeRepSchema (typeRep (proxyOf h)))]
    h@(Cookie'  name) -> [mkParamWithSchema (T.pack (BS8.unpack name)) ParamCookie False (typeRepSchema (typeRep (innerProxyOf h)))]
    Raw               -> []
    Header_ _ _       -> []
extractHeaderParams (FMap _ c)    = extractHeaderParams c
extractHeaderParams (LMap _ c)    = extractHeaderParams c
extractHeaderParams (Apply cf cx) = extractHeaderParams cf ++ extractHeaderParams cx
extractHeaderParams (Pure _)      = []

extractResHeaders :: Codec (Headers ForResponse) i o -> [(Text, Bool, OA.Schema)]
extractResHeaders (Embed hdr) = case hdr of
    h@(Header     key)  -> [(hdrName key, True,  typeRepSchema (typeRep (proxyOf h)))]
    h@(Header'    key)  -> [(hdrName key, False, typeRepSchema (typeRep (innerProxyOf h)))]
    h@(SetCookie  name) -> [(T.pack (BS8.unpack name), True,  typeRepSchema (typeRep (proxyOf h)))]
    h@(SetCookie' name) -> [(T.pack (BS8.unpack name), False, typeRepSchema (typeRep (innerProxyOf h)))]
    Raw                 -> []
    Header_ _ _         -> []
extractResHeaders (FMap _ c)    = extractResHeaders c
extractResHeaders (LMap _ c)    = extractResHeaders c
extractResHeaders (Apply cf cx) = extractResHeaders cf ++ extractResHeaders cx
extractResHeaders (Pure _)      = []

bodySchemaOf :: forall ctx a. IsoJson a => Body ctx (IO a) -> OA.Schema
bodySchemaOf _ = toSchema (Proxy @a)

bodyDefsOf :: forall ctx a. IsoJson a => Body ctx (IO a) -> OA.Definitions OA.Schema
bodyDefsOf _ = execDeclare (declareSchemaRef (Proxy @a)) mempty

extractBodySchema :: Codec (Body ctx) i o -> Maybe OA.Schema
extractBodySchema (Embed body)   = case body of
    Body.Json -> Just (bodySchemaOf body)
    _         -> Nothing
extractBodySchema (FMap _ c)    = extractBodySchema c
extractBodySchema (LMap _ c)    = extractBodySchema c
extractBodySchema (Apply cf cx) = extractBodySchema cf <|> extractBodySchema cx
extractBodySchema (Pure _)      = Nothing

extractBodyDefs :: Codec (Body ctx) i o -> OA.Definitions OA.Schema
extractBodyDefs (Embed body)   = case body of
    Body.Json -> bodyDefsOf body
    _         -> mempty
extractBodyDefs (FMap _ c)    = extractBodyDefs c
extractBodyDefs (LMap _ c)    = extractBodyDefs c
extractBodyDefs (Apply cf cx) = extractBodyDefs cf <> extractBodyDefs cx
extractBodyDefs (Pure _)      = mempty

data ResInfo = ResInfo
    { resStatus     :: HTTP.Status
    , resBodySchema :: Maybe OA.Schema
    , resBodyDefs   :: OA.Definitions OA.Schema
    , resHdrNames   :: [(Text, Bool, OA.Schema)]
    }

extractResInfos :: Responses a -> [ResInfo]
extractResInfos (Only res) =
    [ ResInfo
        { resStatus     = fromMaybe HTTP.status200 (Status.extractStatus res.status.isoCodec)
        , resBodySchema = extractBodySchema res.body.isoCodec
        , resBodyDefs   = extractBodyDefs   res.body.isoCodec
        , resHdrNames   = extractResHeaders res.headers.isoCodec
        }
    ]
extractResInfos (Choice l r) = extractResInfos l ++ extractResInfos r

walkResAltCodec :: Codec Responses i o -> [ResInfo]
walkResAltCodec (Embed ra)    = extractResInfos ra
walkResAltCodec (FMap _ c)    = walkResAltCodec c
walkResAltCodec (LMap _ c)    = walkResAltCodec c
walkResAltCodec (Apply cf cx) = walkResAltCodec cf ++ walkResAltCodec cx
walkResAltCodec (Pure _)      = []

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

endpointToOpenApi :: Contract (Signature m p q h b r) -> OpenApi
endpointToOpenApi (Request
        { method  = IsoCodec methodCodec
        , path    = IsoCodec pathCodec
        , query   = IsoCodec queryCodec
        , headers = IsoCodec headersCodec
        , body    = IsoCodec bodyCodec
        }
    :-> IsoCodec resAlt) =
    let
        stdMeth  = fromMaybe HTTP.GET (Method.extractMethod methodCodec)
        pieces   = walkPath pathCodec []
        qParams  = extractQueryParams queryCodec
        hParams  = extractHeaderParams headersCodec
        resInfos = walkResAltCodec resAlt
        reqBody  = if stdMeth `notElem` [HTTP.GET, HTTP.HEAD]
                   then extractBodySchema bodyCodec
                   else Nothing
        allDefs  = extractBodyDefs bodyCodec
               <> foldMap resBodyDefs resInfos
        op = mempty
            & OA.parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
            & OA.responses  .~ OA.Responses Nothing
                (IHM.fromList
                    [ (HTTP.statusCode (resStatus ri), Inline (mkResResponse ri))
                    | ri <- resInfos
                    ])
            & applyReqBodySchema reqBody
    in mempty
        & OA.info . OA.title   .~ "API"
        & OA.info . OA.version .~ "0.1.0"
        & OA.components . OA.schemas .~ allDefs
        & OA.paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)
