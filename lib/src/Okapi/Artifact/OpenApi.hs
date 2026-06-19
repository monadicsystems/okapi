{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Artifact.OpenApi (endpointToOpenApi, GOpenApiable, openApi) where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Optics.Core ((%), (.~), (?~))
import Data.OpenApi.Optics ()
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi, Operation, Param, ParamLocation (..), PathItem (..), Referenced (..), Response, declareSchemaRef, toSchema)
import Data.OpenApi qualified as OA
import Data.OpenApi.Declare (execDeclare)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Typeable (TypeRep, typeRep)
import Network.HTTP.Media (MediaType)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Protocol.Shared.Body (Body, IsoJson)
import Okapi.Protocol.Shared.Body qualified as Body
import Okapi.Codec (Codec (..), IsoCodec (..))
import Okapi.Protocol.Shared.Headers (ForRequest, ForResponse, Headers (..))
import Okapi.Contract (Signature)
import Okapi.Contract (Contract (..))
import Okapi.Protocol.Request (Request (..))

import Okapi.Protocol.Request.Method qualified as Method
import Okapi.Protocol.Request.Path (Path)
import Okapi.Protocol.Request.Path qualified as Path
import Okapi.Protocol.Request.Query (Query)
import Okapi.Protocol.Request.Query qualified as Query

import Okapi.Protocol.Response qualified as ORes
import Okapi.Protocol.Response.Status qualified as Status
import Okapi.Protocol.Response (Cases, Responses (Responses), traverseResponses)
import Okapi.Data (ToPathData (..))

import Data.Functor.Const (Const (..), getConst)
import Data.List.NonEmpty qualified as NE

data PathPiece = PLit Text | PParam Text OA.Schema

walkPath :: Codec Path i o -> [PathPiece] -> [PathPiece]
walkPath (Lift (Path.Seg_ x))    ps = ps ++ [PLit (toUrlPiece x)]
walkPath (Lift h@(Path.Seg n))   ps = ps ++ [PParam n (typeRepSchema (typeRep (proxyOf h)))]
walkPath (Lift Path.Segs)        ps = ps ++ [PParam "segs" (mempty & #type ?~ OA.OpenApiString)]
walkPath (Lift Path.Raw)         ps = ps
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
extractQueryParams (Lift (Query.Param key))    = [mkParam key ParamQuery True]
extractQueryParams (Lift (Query.Param' key)) = [mkParam key ParamQuery False]
extractQueryParams (Lift (Query.Flag key))   = [mkParam key ParamQuery True]
extractQueryParams (Lift (Query.Flag' key))  = [mkParam key ParamQuery False]
extractQueryParams (Lift (Query.List  style key)) = [mkArrayParam key True  style]
extractQueryParams (Lift (Query.List' style key)) = [mkArrayParam key False style]
extractQueryParams (Lift (Query.DeepObj name _))  = [mkDeepObjectParam name]
extractQueryParams (Lift Query.Raw)             = []
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
    | tr == typeRep (Proxy :: Proxy T.Text)  = mempty & #type ?~ OA.OpenApiString
    | tr == typeRep (Proxy :: Proxy Int)     = mempty & #type ?~ OA.OpenApiInteger
    | tr == typeRep (Proxy :: Proxy Integer) = mempty & #type ?~ OA.OpenApiInteger
    | tr == typeRep (Proxy :: Proxy Bool)    = mempty & #type ?~ OA.OpenApiBoolean
    | otherwise                              = mempty & #type ?~ OA.OpenApiString

extractHeaderParams :: Codec (Headers ForRequest) i o -> [Param]
extractHeaderParams (Lift hdr) = case hdr of
    h@(Header   key)  -> [mkParamWithSchema (hdrName key) ParamHeader True  (typeRepSchema (typeRep (proxyOf h)))]
    h@(Header'  key)  -> [mkParamWithSchema (hdrName key) ParamHeader False (typeRepSchema (typeRep (innerProxyOf h)))]
    h@(Cookie   name) -> [mkParamWithSchema (T.pack (BS8.unpack name)) ParamCookie True  (typeRepSchema (typeRep (proxyOf h)))]
    h@(Cookie'  name) -> [mkParamWithSchema (T.pack (BS8.unpack name)) ParamCookie False (typeRepSchema (typeRep (innerProxyOf h)))]
    Raw               -> []
    Header_ _ _       -> []
    -- structured headers degrade to a string-typed header param (lossy); content-type
    -- is represented via the content map instead, so skip it here.
    Structured _ name _
        | name == "content-type" -> []
        | otherwise              -> [mkParamWithSchema (hdrName name) ParamHeader True (mempty & #type ?~ OA.OpenApiString)]
extractHeaderParams (FMap _ c)    = extractHeaderParams c
extractHeaderParams (LMap _ c)    = extractHeaderParams c
extractHeaderParams (Apply cf cx) = extractHeaderParams cf ++ extractHeaderParams cx
extractHeaderParams (Pure _)      = []

extractResHeaders :: Codec (Headers ForResponse) i o -> [(Text, Bool, OA.Schema)]
extractResHeaders (Lift hdr) = case hdr of
    h@(Header     key)  -> [(hdrName key, True,  typeRepSchema (typeRep (proxyOf h)))]
    h@(Header'    key)  -> [(hdrName key, False, typeRepSchema (typeRep (innerProxyOf h)))]
    h@(SetCookie  name) -> [(T.pack (BS8.unpack name), True,  typeRepSchema (typeRep (proxyOf h)))]
    h@(SetCookie' name) -> [(T.pack (BS8.unpack name), False, typeRepSchema (typeRep (innerProxyOf h)))]
    Raw                 -> []
    Header_ _ _         -> []
    Structured _ name _
        | name == "content-type" -> []
        | otherwise              -> [(hdrName name, True, mempty & #type ?~ OA.OpenApiString)]
extractResHeaders (FMap _ c)    = extractResHeaders c
extractResHeaders (LMap _ c)    = extractResHeaders c
extractResHeaders (Apply cf cx) = extractResHeaders cf ++ extractResHeaders cx
extractResHeaders (Pure _)      = []

bodySchemaOf :: forall ctx a. IsoJson a => Body ctx (IO a) -> OA.Schema
bodySchemaOf _ = toSchema (Proxy @a)

bodyDefsOf :: forall ctx a. IsoJson a => Body ctx (IO a) -> OA.Definitions OA.Schema
bodyDefsOf _ = execDeclare (declareSchemaRef (Proxy @a)) mempty

extractBodySchema :: Codec (Body ctx) i o -> Maybe OA.Schema
extractBodySchema (Lift body)   = case body of
    Body.Json -> Just (bodySchemaOf body)
    _         -> Nothing
extractBodySchema (FMap _ c)    = extractBodySchema c
extractBodySchema (LMap _ c)    = extractBodySchema c
extractBodySchema (Apply cf cx) = extractBodySchema cf <|> extractBodySchema cx
extractBodySchema (Pure _)      = Nothing

extractBodyDefs :: Codec (Body ctx) i o -> OA.Definitions OA.Schema
extractBodyDefs (Lift body)   = case body of
    Body.Json -> bodyDefsOf body
    _         -> mempty
extractBodyDefs (FMap _ c)    = extractBodyDefs c
extractBodyDefs (LMap _ c)    = extractBodyDefs c
extractBodyDefs (Apply cf cx) = extractBodyDefs cf <> extractBodyDefs cx
extractBodyDefs (Pure _)      = mempty

data ResInfo = ResInfo
    { resStatus     :: HTTP.Status
    , resMediaType  :: Maybe BS8.ByteString
    , resBodySchema :: Maybe OA.Schema
    , resBodyDefs   :: OA.Definitions OA.Schema
    , resHdrNames   :: [(Text, Bool, OA.Schema)]
    }

resInfoOf :: ORes.Response IsoCodec s h b -> ResInfo
resInfoOf res = ResInfo
    { resStatus     = fromMaybe HTTP.status200 (Status.extractStatus res.status.isoCodec)
    , resMediaType  = Body.bodyMediaType res.body.isoCodec
    , resBodySchema = extractBodySchema res.body.isoCodec
    , resBodyDefs   = extractBodyDefs   res.body.isoCodec
    , resHdrNames   = extractResHeaders res.headers.isoCodec
    }

-- | One 'ResInfo' per constructor: traverse each branch's codec to its (status, schemas).
extractResInfos :: Cases responses => Responses IsoCodec responses -> [ResInfo]
extractResInfos (Responses cs) =
    map (getConst . traverseResponses @IsoCodec @IsoCodec (\c -> Const (resInfoOf c))) (NE.toList cs)

hdrName :: HTTP.HeaderName -> Text
hdrName = T.pack . BS8.unpack . CI.original

mkParam :: Text -> ParamLocation -> Bool -> Param
mkParam n loc req_ = mempty
    & #name     .~ n
    & #in      .~ loc
    & #required ?~ req_

mkParamWithSchema :: Text -> ParamLocation -> Bool -> OA.Schema -> Param
mkParamWithSchema n loc req_ sc = mempty
    & #name     .~ n
    & #in      .~ loc
    & #required ?~ req_
    & #schema   ?~ Inline sc

-- | An array query parameter with OpenAPI @style@/@explode@ matching the 'Query.ArrayStyle'.
mkArrayParam :: Text -> Bool -> Query.ArrayStyle -> Param
mkArrayParam n req_ style = mempty
    & #name     .~ n
    & #in      .~ ParamQuery
    & #required ?~ req_
    & #style    ?~ st
    & #explode  ?~ ex
    & #schema   ?~ Inline arraySchema
  where
    arraySchema = mempty
        & #type  ?~ OA.OpenApiArray
        & #items ?~ OA.OpenApiItemsObject (Inline (mempty & #type ?~ OA.OpenApiString))
    (st, ex) = case style of
        Query.Exploded       -> (OA.StyleForm,           True)
        Query.CommaDelimited -> (OA.StyleForm,           False)
        Query.SpaceDelimited -> (OA.StyleSpaceDelimited, False)
        Query.PipeDelimited  -> (OA.StylePipeDelimited,  False)

-- | A @deepObject@ object query parameter (OpenAPI style=deepObject, explode=true).
mkDeepObjectParam :: Text -> Param
mkDeepObjectParam n = mempty
    & #name     .~ n
    & #in      .~ ParamQuery
    & #required ?~ True
    & #style    ?~ OA.StyleDeepObject
    & #explode  ?~ True
    & #schema   ?~ Inline (mempty & #type ?~ OA.OpenApiObject)

mkResResponse :: ResInfo -> Response
mkResResponse ri =
    mempty
    & #description .~ T.pack (show (HTTP.statusCode (resStatus ri)))
    & applyResBodySchema (resMediaType ri) (resBodySchema ri)
    & applyResHeaders (resHdrNames ri)

applyResBodySchema :: Maybe BS8.ByteString -> Maybe OA.Schema -> Response -> Response
applyResBodySchema _  Nothing   r = r
applyResBodySchema mt (Just sc) r = r
    & #content .~ IHM.singleton (mediaKey mt)
        (mempty & #schema ?~ Inline sc)

applyResHeaders :: [(Text, Bool, OA.Schema)] -> Response -> Response
applyResHeaders [] r = r
applyResHeaders hs r = r
    & #headers .~ IHM.fromList
        [ (name, Inline (mempty & #required ?~ req_ & #schema ?~ Inline sc))
        | (name, req_, sc) <- hs
        ]

applyReqBodySchema :: Maybe BS8.ByteString -> Maybe OA.Schema -> Operation -> Operation
applyReqBodySchema _  Nothing   op = op
applyReqBodySchema mt (Just sc) op = op
    & #requestBody ?~ Inline
        ( mempty & #content .~ IHM.singleton (mediaKey mt)
            (mempty & #schema ?~ Inline sc)
        )

-- | The OpenAPI @content@ media-type key for a body, defaulting to @application/json@.
mediaKey :: Maybe BS8.ByteString -> MediaType
mediaKey = maybe "application/json" (fromString . BS8.unpack)

setMethod :: HTTP.StdMethod -> Operation -> PathItem -> PathItem
setMethod HTTP.GET    op pi_ = pi_ { _pathItemGet    = Just op }
setMethod HTTP.POST   op pi_ = pi_ { _pathItemPost   = Just op }
setMethod HTTP.PUT    op pi_ = pi_ { _pathItemPut    = Just op }
setMethod HTTP.DELETE op pi_ = pi_ { _pathItemDelete = Just op }
setMethod _           op pi_ = pi_ { _pathItemGet    = Just op }

endpointToOpenApi :: Cases responses => Contract (Signature method path query headers body responses) -> OpenApi
endpointToOpenApi (Request
        { method  = IsoCodec methodCodec
        , path    = IsoCodec pathCodec
        , query   = IsoCodec queryCodec
        , headers = IsoCodec headersCodec
        , body    = IsoCodec bodyCodec
        }
    :-> resAlt) =
    let
        stdMeth  = fromMaybe HTTP.GET (Method.extractMethod methodCodec)
        pieces   = walkPath pathCodec []
        qParams  = extractQueryParams queryCodec
        hParams  = extractHeaderParams headersCodec
        resInfos = extractResInfos resAlt
        reqBody  = if stdMeth `notElem` [HTTP.GET, HTTP.HEAD]
                   then extractBodySchema bodyCodec
                   else Nothing
        allDefs  = extractBodyDefs bodyCodec
               <> foldMap resBodyDefs resInfos
        op = mempty
            & #parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
            & #responses  .~ OA.Responses Nothing
                (IHM.fromList
                    [ (HTTP.statusCode (resStatus ri), Inline (mkResResponse ri))
                    | ri <- resInfos
                    ])
            & applyReqBodySchema (Body.bodyMediaType bodyCodec) reqBody
    in mempty
        & #info % #title   .~ "API"
        & #info % #version .~ "0.1.0"
        & #components % #schemas .~ allDefs
        & #paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)


-- ── GOpenApiable ─────────────────────────────────────────────────────────────

class GOpenApiable (epF :: Type -> Type) where
    gOpenApi :: epF () -> OpenApi

instance GOpenApiable epF => GOpenApiable (D1 dm epF) where
    gOpenApi (M1 ep) = gOpenApi @epF ep

instance GOpenApiable epF => GOpenApiable (C1 cm epF) where
    gOpenApi (M1 ep) = gOpenApi @epF ep

instance (GOpenApiable epL, GOpenApiable epR) => GOpenApiable (epL :*: epR) where
    gOpenApi (epL :*: epR) = gOpenApi @epL epL <> gOpenApi @epR epR

instance Cases responses => GOpenApiable (S1 sm (Rec0 (Contract (Signature method path query headers body responses)))) where
    gOpenApi (M1 (K1 ep)) = endpointToOpenApi ep

-- | Derive an OpenAPI 3.0 document from a record of contracts.
openApi ::
    forall server.
    ( Generic (server Contract)
    , GOpenApiable (Rep (server Contract))
    ) =>
    server Contract ->
    OpenApi
openApi = gOpenApi @(Rep (server Contract)) . from
