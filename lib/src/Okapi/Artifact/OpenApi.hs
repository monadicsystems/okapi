{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Artifact.OpenApi (endpointToOpenApi, GenericOAPI, openApi) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi, Operation, Param, ParamLocation (..), PathItem (..), Referenced (..), Response, declareSchemaRef, toSchema)
import Data.OpenApi qualified as OA
import Data.OpenApi.Declare (execDeclare)
import Data.OpenApi.Optics ()
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, Rep, S1, (:*:) (..))
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), IsoCodec (..))
import Okapi.Contract (Contract (..), Signature)
import Okapi.Protocol.Body (Body (..), IsoJson)
import Okapi.Protocol.Body qualified as Body
import Okapi.Data (Info (..), Iso (..))
import Okapi.Protocol.Headers (Headers (..))
import Okapi.Protocol.Headers qualified as H
import Okapi.Protocol.Request (Request (..))
import Optics.Core ((%), (.~), (?~))

import Okapi.Protocol.Request.Method qualified as Method
import Okapi.Protocol.Request.Path (Path)
import Okapi.Protocol.Request.Path qualified as Path
import Okapi.Protocol.Request.Query (Query)
import Okapi.Protocol.Request.Query qualified as Query

import Okapi.Protocol.Response (Cases, Responses (Responses), traverseResponses)
import Okapi.Protocol.Response qualified as ORes
import Okapi.Protocol.Response.Status qualified as Status

import Data.Functor.Const (Const (..), getConst)
import Data.List.NonEmpty qualified as NE

data PathPiece = PLit Text | PParam Text OA.Schema

walkPath :: Codec Path i o -> [PathPiece] -> [PathPiece]
walkPath (Lift (Path.Seg_ vIso x)) ps = ps ++ [PLit (vIso.encode x)]
walkPath (Lift (Path.Seg n vIso)) ps  = ps ++ [PParam n (infoSchema vIso.info)]
walkPath (Lift (Path.Segs vIso)) ps   = ps ++ [PParam "segs" (infoSchema vIso.info)]
walkPath (Lift Path.Raw) ps           = ps
walkPath (FMap _ c) ps                = walkPath c ps
walkPath (LMap _ c) ps                = walkPath c ps
walkPath (Apply cf cx) ps             = walkPath cx (walkPath cf ps)
walkPath (Pure _) ps                  = ps

pathTemplate :: [PathPiece] -> FilePath
pathTemplate pieces = "/" <> intercalate "/" (map piece pieces)
  where
    piece (PLit t)    = T.unpack t
    piece (PParam n _) = "{" <> T.unpack n <> "}"

pathOAParams :: [PathPiece] -> [Param]
pathOAParams pieces =
    [ mkParamWithSchema name ParamPath True sc
    | PParam name sc <- pieces
    ]

extractQueryParams :: Codec Query i o -> [Param]
extractQueryParams (Lift (Query.Param key vIso))       = [mkParamWithSchema key ParamQuery True (infoSchema vIso.info)]
extractQueryParams (Lift (Query.Param' key vIso))      = [mkParamWithSchema key ParamQuery False (infoSchema vIso.info)]
extractQueryParams (Lift (Query.Param_ key vIso _))    = [mkParamWithSchema key ParamQuery True (infoSchema vIso.info)]
extractQueryParams (Lift (Query.Flag key))             = [mkParam key ParamQuery True]
extractQueryParams (Lift (Query.Flag' key))            = [mkParam key ParamQuery False]
extractQueryParams (Lift (Query.List style key _))     = [mkArrayParam key True style]
extractQueryParams (Lift (Query.List' style key _))    = [mkArrayParam key False style]
extractQueryParams (Lift Query.Raw)                    = []
extractQueryParams (FMap _ c)                          = extractQueryParams c
extractQueryParams (LMap _ c)                          = extractQueryParams c
extractQueryParams (Apply cf cx)                       = extractQueryParams cf ++ extractQueryParams cx
extractQueryParams (Pure _)                            = []

infoSchema :: Info -> OA.Schema
infoSchema (Info ty fmt) = withFmt (mempty & #type ?~ oaType ty)
  where
    oaType "integer" = OA.OpenApiInteger
    oaType "number"  = OA.OpenApiNumber
    oaType "boolean" = OA.OpenApiBoolean
    oaType _         = OA.OpenApiString
    withFmt s = maybe s (\f -> s & #format ?~ f) fmt

extractHeaderParams :: Codec (Headers ctx) i o -> [Param]
extractHeaderParams (Lift hdr) = case hdr of
    H.Field key vIso      -> [mkParamWithSchema (hdrName key) ParamHeader True (infoSchema vIso.info)]
    H.Field' key vIso     -> [mkParamWithSchema (hdrName key) ParamHeader False (infoSchema vIso.info)]
    H.Raw                 -> []
    H.Field_ _ _          -> []
    H.FieldStructured n _ -> [mkParamWithSchema (hdrName n) ParamHeader True (mempty & #type ?~ OA.OpenApiString)]
    H.Cookie{}            -> []
    H.Cookie'{}           -> []
    H.SetCookie{}         -> []
extractHeaderParams (FMap _ c)    = extractHeaderParams c
extractHeaderParams (LMap _ c)    = extractHeaderParams c
extractHeaderParams (Apply cf cx) = extractHeaderParams cf ++ extractHeaderParams cx
extractHeaderParams (Pure _)      = []

extractResHeaders :: Codec (Headers ctx) i o -> [(Text, Bool, OA.Schema)]
extractResHeaders (Lift hdr) = case hdr of
    H.Field key vIso      -> [(hdrName key, True,  infoSchema vIso.info)]
    H.Field' key vIso     -> [(hdrName key, False, infoSchema vIso.info)]
    H.Raw                 -> []
    H.Field_ _ _          -> []
    H.FieldStructured n _ -> [(hdrName n, True, mempty & #type ?~ OA.OpenApiString)]
    H.Cookie{}            -> []
    H.Cookie'{}           -> []
    H.SetCookie{}         -> []
extractResHeaders (FMap _ c)    = extractResHeaders c
extractResHeaders (LMap _ c)    = extractResHeaders c
extractResHeaders (Apply cf cx) = extractResHeaders cf ++ extractResHeaders cx
extractResHeaders (Pure _)      = []

extractContentType :: Codec (Headers ctx) i o -> Maybe ByteString
extractContentType (Lift (Field_ k v)) | k == "content-type" = Just v
extractContentType (FMap _ c)    = extractContentType c
extractContentType (LMap _ c)    = extractContentType c
extractContentType (Apply cf cx) = extractContentType cf <|> extractContentType cx
extractContentType _             = Nothing

bodySchemaOf :: forall (b :: Type -> Type -> Type) ctx a. IsoJson a => b ctx (IO a) -> OA.Schema
bodySchemaOf _ = toSchema (Proxy @a)

bodyDefsOf :: forall (b :: Type -> Type -> Type) ctx a. IsoJson a => b ctx (IO a) -> OA.Definitions OA.Schema
bodyDefsOf _ = execDeclare (declareSchemaRef (Proxy @a)) mempty

extractBodySchema :: Codec (Body ctx) i o -> Maybe OA.Schema
extractBodySchema (Lift body) = case body of
    Body.Json -> Just (bodySchemaOf body)
    _         -> Nothing
extractBodySchema (FMap _ c)    = extractBodySchema c
extractBodySchema (LMap _ c)    = extractBodySchema c
extractBodySchema (Apply cf cx) = extractBodySchema cf <|> extractBodySchema cx
extractBodySchema (Pure _)      = Nothing

extractBodyDefs :: Codec (Body ctx) i o -> OA.Definitions OA.Schema
extractBodyDefs (Lift body) = case body of
    Body.Json -> bodyDefsOf body
    _         -> mempty
extractBodyDefs (FMap _ c)    = extractBodyDefs c
extractBodyDefs (LMap _ c)    = extractBodyDefs c
extractBodyDefs (Apply cf cx) = extractBodyDefs cf <> extractBodyDefs cx
extractBodyDefs (Pure _)      = mempty

data ResInfo = ResInfo
    { resStatus     :: HTTP.Status
    , resMediaType  :: Maybe ByteString
    , resBodySchema :: Maybe OA.Schema
    , resBodyDefs   :: OA.Definitions OA.Schema
    , resHdrNames   :: [(Text, Bool, OA.Schema)]
    }

resInfoOf :: ORes.Response IsoCodec s h b -> ResInfo
resInfoOf res =
    ResInfo
        { resStatus     = fromMaybe HTTP.status200 (Status.extractStatus res.status.isoCodec)
        , resMediaType  = extractContentType res.headers.isoCodec
        , resBodySchema = extractBodySchema res.body.isoCodec
        , resBodyDefs   = extractBodyDefs res.body.isoCodec
        , resHdrNames   = extractResHeaders res.headers.isoCodec
        }

extractResInfos :: Cases responses => Responses IsoCodec responses -> [ResInfo]
extractResInfos (Responses cs) =
    map (getConst . traverseResponses @IsoCodec @IsoCodec (\c -> Const (resInfoOf c))) (NE.toList cs)

hdrName :: HTTP.HeaderName -> Text
hdrName = T.pack . BS8.unpack . CI.original

mkParam :: Text -> ParamLocation -> Bool -> Param
mkParam n loc req_ =
    mempty
        & #name .~ n
        & #in .~ loc
        & #required ?~ req_

mkParamWithSchema :: Text -> ParamLocation -> Bool -> OA.Schema -> Param
mkParamWithSchema n loc req_ sc =
    mempty
        & #name .~ n
        & #in .~ loc
        & #required ?~ req_
        & #schema ?~ Inline sc

mkArrayParam :: Text -> Bool -> Query.ArrayStyle -> Param
mkArrayParam n req_ style =
    mempty
        & #name .~ n
        & #in .~ ParamQuery
        & #required ?~ req_
        & #style ?~ st
        & #explode ?~ ex
        & #schema ?~ Inline arraySchema
  where
    arraySchema =
        mempty
            & #type ?~ OA.OpenApiArray
            & #items ?~ OA.OpenApiItemsObject (Inline (mempty & #type ?~ OA.OpenApiString))
    (st, ex) = case style of
        Query.Exploded       -> (OA.StyleForm, True)
        Query.CommaDelimited -> (OA.StyleForm, False)
        Query.SpaceDelimited -> (OA.StyleSpaceDelimited, False)
        Query.PipeDelimited  -> (OA.StylePipeDelimited, False)

mkResResponse :: ResInfo -> Response
mkResResponse ri =
    mempty
        & #description .~ T.pack (show (HTTP.statusCode (resStatus ri)))
        & applyResBodySchema (resMediaType ri) (resBodySchema ri)
        & applyResHeaders (resHdrNames ri)

applyResBodySchema :: Maybe ByteString -> Maybe OA.Schema -> Response -> Response
applyResBodySchema _ Nothing r = r
applyResBodySchema mt (Just sc) r =
    r & #content .~ IHM.singleton (mediaKey mt) (mempty & #schema ?~ Inline sc)

applyResHeaders :: [(Text, Bool, OA.Schema)] -> Response -> Response
applyResHeaders [] r = r
applyResHeaders hs r =
    r & #headers .~ IHM.fromList
        [ (name, Inline (mempty & #required ?~ req_ & #schema ?~ Inline sc))
        | (name, req_, sc) <- hs
        ]

applyReqBodySchema :: Maybe ByteString -> Maybe OA.Schema -> Operation -> Operation
applyReqBodySchema _ Nothing op = op
applyReqBodySchema mt (Just sc) op =
    op & #requestBody ?~ Inline
        ( mempty & #content .~ IHM.singleton (mediaKey mt) (mempty & #schema ?~ Inline sc) )

mediaKey :: Maybe ByteString -> MediaType
mediaKey = maybe "application/json" (fromString . BS8.unpack)

setMethod :: HTTP.StdMethod -> Operation -> PathItem -> PathItem
setMethod HTTP.GET    op pi_ = pi_{_pathItemGet    = Just op}
setMethod HTTP.POST   op pi_ = pi_{_pathItemPost   = Just op}
setMethod HTTP.PUT    op pi_ = pi_{_pathItemPut    = Just op}
setMethod HTTP.DELETE op pi_ = pi_{_pathItemDelete = Just op}
setMethod _           op pi_ = pi_{_pathItemGet    = Just op}

endpointToOpenApi :: Cases responses => Contract (Signature method path query headers body responses) -> OpenApi
endpointToOpenApi
    ( Request
            { method  = IsoCodec methodCodec
            , path    = IsoCodec pathCodec
            , query   = IsoCodec queryCodec
            , headers = IsoCodec headersCodec
            , body    = IsoCodec bodyCodec
            }
            :-> resAlt
        ) =
        let
            stdMeth  = fromMaybe HTTP.GET (Method.extractMethod methodCodec)
            pieces   = walkPath pathCodec []
            qParams  = extractQueryParams queryCodec
            hParams  = extractHeaderParams headersCodec
            resInfos = extractResInfos resAlt
            reqBody  =
                if stdMeth `notElem` [HTTP.GET, HTTP.HEAD]
                    then extractBodySchema bodyCodec
                    else Nothing
            allDefs =
                extractBodyDefs bodyCodec
                    <> foldMap resBodyDefs resInfos
            op =
                mempty
                    & #parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
                    & #responses .~ OA.Responses Nothing
                        ( IHM.fromList
                            [ (HTTP.statusCode (resStatus ri), Inline (mkResResponse ri))
                            | ri <- resInfos
                            ]
                        )
                    & applyReqBodySchema (extractContentType headersCodec) reqBody
         in
            mempty
                & #info % #title .~ "API"
                & #info % #version .~ "0.1.0"
                & #components % #schemas .~ allDefs
                & #paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)

class GenericOAPI (epF :: Type -> Type) where
    gOpenApi :: epF () -> OpenApi

instance GenericOAPI epF => GenericOAPI (D1 dm epF) where
    gOpenApi (M1 ep) = gOpenApi @epF ep

instance GenericOAPI epF => GenericOAPI (C1 cm epF) where
    gOpenApi (M1 ep) = gOpenApi @epF ep

instance (GenericOAPI epL, GenericOAPI epR) => GenericOAPI (epL :*: epR) where
    gOpenApi (epL :*: epR) = gOpenApi @epL epL <> gOpenApi @epR epR

instance Cases responses => GenericOAPI (S1 sm (Rec0 (Contract (Signature method path query headers body responses)))) where
    gOpenApi (M1 (K1 ep)) = endpointToOpenApi ep

openApi ::
    forall server.
    ( Generic (server Contract)
    , GenericOAPI (Rep (server Contract))
    ) =>
    server Contract ->
    OpenApi
openApi = gOpenApi @(Rep (server Contract)) . from
