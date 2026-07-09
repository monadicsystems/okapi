{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Artifact.OpenApi (endpointToOpenApi, GenericOAPI, openApi) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.HashSet.InsOrd qualified as IHS
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
import Data.Functor.Const (Const (..), getConst)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, Rep, S1, (:*:) (..))
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types qualified as HTTP
import Okapi.Mode.Forest (Forest ((:->), (:-<)), Shape, stripTags, collectTags)
import Okapi.Mode.Forest qualified as Forest
import Okapi.Tree (Info (..), Leaf (..), Tag (..))
import Okapi.Record.Tree qualified as Tree
import Okapi.HTTP.Request.Method qualified as Method
import Okapi.HTTP.Request.Path (Path)
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query (Query)
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.HTTP.Request.Headers qualified as ReqH
import Okapi.HTTP.Request.Body qualified as ReqBody
import Okapi.HTTP.Response.Headers qualified as ResH
import Okapi.HTTP.Response.Body qualified as ResBody
import Okapi.HTTP.Response.Status qualified as Status
import Okapi.HTTP.Responses (Cases, Responses)
import Okapi.HTTP.Responses qualified as Resps
import Okapi.Tree (Tree (..))
import Optics.Core ((%), (.~), (?~), (%~))

-- | Fold 'Tag's onto an 'OA.Schema' — used for node-level metadata (path
--   segments, response headers) where no separate 'Param' is built.
--   'Group' and 'Extension' have no target here (see 'Okapi.Tree.Tag').
applyTagsToSchema :: [Tag] -> OA.Schema -> OA.Schema
applyTagsToSchema tags sc = foldl' applyOne sc tags
  where
    applyOne s (Description d) = s & #description ?~ d
    applyOne s (Example v)     = s & #example ?~ v
    applyOne s Deprecated      = s & #deprecated ?~ True
    applyOne s (Group _)       = s
    applyOne s (Extension _ _) = s

-- | Fold 'Tag's onto a 'Param' — used for query\/header parameters, which
--   are built directly (no separate schema-then-param step).
applyTagsToParam :: [Tag] -> Param -> Param
applyTagsToParam tags p = foldl' applyOne p tags
  where
    applyOne pr (Description d) = pr & #description ?~ d
    applyOne pr (Example v)     = pr & #example ?~ v
    applyOne pr Deprecated      = pr & #deprecated ?~ True
    applyOne pr (Group _)       = pr
    applyOne pr (Extension _ _) = pr

-- | Fold 'Tag's onto an 'Operation' — the contract-level (whole endpoint)
--   case. 'Example' has no target here (OpenAPI has no per-operation
--   example; examples live on 'Param'\/'OA.Schema'\/'Response').
applyTagsToOperation :: [Tag] -> Operation -> Operation
applyTagsToOperation tags op = foldl' applyOne op tags
  where
    applyOne o (Description d) = o & #description ?~ d
    applyOne o Deprecated       = o & #deprecated ?~ True
    applyOne o (Group g)        = o & #tags %~ IHS.insert g
    applyOne o (Example _)      = o
    applyOne o (Extension _ _)  = o

data PathPiece = PLit Text | PParam Text OA.Schema

-- | Threads an accumulated @[Tag]@ (from any enclosing 'Okapi.Tree.annotate'
--   layers) down to each leaf, folding it into that leaf's 'OA.Schema'
--   right there — not accumulated bottom-up and merged after, which would
--   get nested 'annotate' calls' merge order backwards (see
--   'Okapi.Mode.Forest.collectTags').
walkPath :: [Tag] -> Tree Path i o -> [PathPiece]
walkPath _    (Node (Path.Seg_ vLeaf x)) = [PLit (vLeaf.encode x)]
walkPath tags (Node (Path.Seg n vLeaf))  = [PParam n (applyTagsToSchema tags (infoSchema vLeaf.info))]
walkPath tags (Node (Path.Segs vLeaf))   = [PParam "segs" (applyTagsToSchema tags (infoSchema vLeaf.info))]
walkPath _    (Node Path.Raw)            = []
walkPath tags (FMap _ c)                 = walkPath tags c
walkPath tags (LMap _ c)                 = walkPath tags c
walkPath tags (Apply cf cx)              = walkPath tags cf <> walkPath tags cx
walkPath _    (Pure _)                   = []
walkPath tags (Annotate ts c)            = walkPath (tags <> ts) c

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

extractQueryParams :: [Tag] -> Tree Query i o -> [Param]
extractQueryParams tags (Node qry) = map (applyTagsToParam tags) $ case qry of
    Query.Param  key vLeaf   -> [mkParamWithSchema key ParamQuery True  (infoSchema vLeaf.info)]
    Query.Param' key vLeaf   -> [mkParamWithSchema key ParamQuery False (infoSchema vLeaf.info)]
    Query.Param_ key vLeaf _ -> [mkParamWithSchema key ParamQuery True  (infoSchema vLeaf.info)]
    Query.Flag   key         -> [mkParam key ParamQuery True]
    Query.Flag'  key         -> [mkParam key ParamQuery False]
    Query.List  style key _  -> [mkArrayParam key True  style]
    Query.List' style key _  -> [mkArrayParam key False style]
    Query.Raw                -> []
extractQueryParams tags (FMap _ c)     = extractQueryParams tags c
extractQueryParams tags (LMap _ c)     = extractQueryParams tags c
extractQueryParams tags (Apply cf cx)  = extractQueryParams tags cf ++ extractQueryParams tags cx
extractQueryParams _    (Pure _)       = []
extractQueryParams tags (Annotate ts c) = extractQueryParams (tags <> ts) c

infoSchema :: Info -> OA.Schema
infoSchema (Info ty fmt) = withFmt (mempty & #type ?~ oaType ty)
  where
    oaType "integer" = OA.OpenApiInteger
    oaType "number"  = OA.OpenApiNumber
    oaType "boolean" = OA.OpenApiBoolean
    oaType _         = OA.OpenApiString
    withFmt s = maybe s (\f -> s & #format ?~ f) fmt

extractReqHeaderParams :: [Tag] -> Tree ReqH.Headers i o -> [Param]
extractReqHeaderParams tags (Node hdr) = map (applyTagsToParam tags) $ case hdr of
    ReqH.Field  key vLeaf    -> [mkParamWithSchema (hdrName key) ParamHeader True  (infoSchema vLeaf.info)]
    ReqH.Field' key vLeaf    -> [mkParamWithSchema (hdrName key) ParamHeader False (infoSchema vLeaf.info)]
    ReqH.Raw                 -> []
    ReqH.Field_ _ _          -> []
    ReqH.FieldRFC9651 n _    -> [mkParamWithSchema (hdrName n) ParamHeader True (mempty & #type ?~ OA.OpenApiString)]
    ReqH.Cookie  _ _         -> []
    ReqH.Cookie' _ _         -> []
extractReqHeaderParams tags (FMap _ c)     = extractReqHeaderParams tags c
extractReqHeaderParams tags (LMap _ c)     = extractReqHeaderParams tags c
extractReqHeaderParams tags (Apply cf cx)  = extractReqHeaderParams tags cf ++ extractReqHeaderParams tags cx
extractReqHeaderParams _    (Pure _)       = []
extractReqHeaderParams tags (Annotate ts c) = extractReqHeaderParams (tags <> ts) c

extractResHeaders :: [Tag] -> Tree ResH.Headers i o -> [(Text, Bool, OA.Schema)]
extractResHeaders tags (Node hdr) = map applyTags $ case hdr of
    ResH.Field  key vLeaf    -> [(hdrName key, True,  infoSchema vLeaf.info)]
    ResH.Field' key vLeaf    -> [(hdrName key, False, infoSchema vLeaf.info)]
    ResH.Raw                 -> []
    ResH.Field_ _ _          -> []
    ResH.FieldRFC9651 n _    -> [(hdrName n, True, mempty & #type ?~ OA.OpenApiString)]
    ResH.SetCookie _ _ _     -> []
  where
    applyTags (name, req_, sc) = (name, req_, applyTagsToSchema tags sc)
extractResHeaders tags (FMap _ c)     = extractResHeaders tags c
extractResHeaders tags (LMap _ c)     = extractResHeaders tags c
extractResHeaders tags (Apply cf cx)  = extractResHeaders tags cf ++ extractResHeaders tags cx
extractResHeaders _    (Pure _)       = []
extractResHeaders tags (Annotate ts c) = extractResHeaders (tags <> ts) c

extractReqContentType :: Tree ReqH.Headers i o -> Maybe ByteString
extractReqContentType (Node (ReqH.Field_ k v)) | k == "content-type" = Just v
extractReqContentType (FMap _ c)    = extractReqContentType c
extractReqContentType (LMap _ c)    = extractReqContentType c
extractReqContentType (Apply cf cx) = extractReqContentType cf <|> extractReqContentType cx
extractReqContentType (Annotate _ c) = extractReqContentType c
extractReqContentType _             = Nothing

extractResContentType :: Tree ResH.Headers i o -> Maybe ByteString
extractResContentType (Node (ResH.Field_ k v)) | k == "content-type" = Just v
extractResContentType (FMap _ c)    = extractResContentType c
extractResContentType (LMap _ c)    = extractResContentType c
extractResContentType (Apply cf cx) = extractResContentType cf <|> extractResContentType cx
extractResContentType (Annotate _ c) = extractResContentType c
extractResContentType _             = Nothing

bodySchemaOf :: forall (b :: Type -> Type) a. ReqBody.IsoJson a => b (IO a) -> OA.Schema
bodySchemaOf _ = toSchema (Proxy @a)

bodyDefsOf :: forall (b :: Type -> Type) a. ReqBody.IsoJson a => b (IO a) -> OA.Definitions OA.Schema
bodyDefsOf _ = execDeclare (declareSchemaRef (Proxy @a)) mempty

extractReqBodySchema :: ReqBody.Body a -> Maybe OA.Schema
extractReqBodySchema body = case body of
    ReqBody.Json -> Just (bodySchemaOf body)
    _            -> Nothing

extractReqBodyDefs :: ReqBody.Body a -> OA.Definitions OA.Schema
extractReqBodyDefs body = case body of
    ReqBody.Json -> bodyDefsOf body
    _            -> mempty

extractResBodySchema :: ResBody.Body a -> Maybe OA.Schema
extractResBodySchema body = case body of
    ResBody.Json -> Just (bodySchemaOf body)
    _            -> Nothing

extractResBodyDefs :: ResBody.Body a -> OA.Definitions OA.Schema
extractResBodyDefs body = case body of
    ResBody.Json -> bodyDefsOf body
    _            -> mempty

data ResInfo = ResInfo
    { resStatus     :: HTTP.Status
    , resMediaType  :: Maybe ByteString
    , resBodySchema :: Maybe OA.Schema
    , resBodyDefs   :: OA.Definitions OA.Schema
    , resHdrNames   :: [(Text, Bool, OA.Schema)]
    }

resStatusOf :: Status.Status s -> HTTP.Status
resStatusOf Status.Raw        = HTTP.status200
resStatusOf (Status.Status ks) = Status.knownStatusToHTTP ks

resInfoOf :: Tree.Response s h b -> ResInfo
resInfoOf res = ResInfo
    { resStatus     = resStatusOf res.status
    , resMediaType  = extractResContentType res.headers
    , resBodySchema = extractResBodySchema res.body
    , resBodyDefs   = extractResBodyDefs res.body
    , resHdrNames   = extractResHeaders [] res.headers
    }

extractResInfos :: Cases responses => Responses Tree.Response responses -> [ResInfo]
extractResInfos rs =
    map
        (getConst . Resps.traverseResponses @Tree.Response @Tree.Response (\c -> Const (resInfoOf c)))
        (NE.toList (Resps.getResponses rs))

methodStdOf :: Method.Method m -> Maybe HTTP.StdMethod
methodStdOf Method.Raw        = Nothing
methodStdOf (Method.Method km) = Just (Method.knownMethodToStd km)

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

-- $setup
-- >>> :set -XOverloadedLabels
-- >>> import Okapi.Mode.Forest qualified as Forest
-- >>> import Okapi.Record.Tree qualified as Tree
-- >>> import Okapi.HTTP.Request.Path (Path, segment)
-- >>> import Okapi.HTTP.Request.Query qualified as Query
-- >>> import Okapi.HTTP.Request.Headers qualified as ReqH
-- >>> import Okapi.HTTP.Request.Body qualified as ReqBody
-- >>> import Okapi.HTTP.Request.Method qualified as Method
-- >>> import Okapi.HTTP.Response qualified as Res
-- >>> import Okapi.Tree (Leaf, Tag (..), annotate, integer)
-- >>> import Data.HashMap.Strict.InsOrd qualified as IHM
-- >>> import Data.OpenApi (Referenced (..))
-- >>> import Optics.Core ((^.))
-- >>> let reqTree = Tree.Request { Tree.method = Method.method Method.GET, Tree.path = annotate [Description "the user id"] (segment "userId" (integer :: Leaf Path Integer)), Tree.query = Query.raw, Tree.headers = ReqH.raw, Tree.body = ReqBody.raw }
-- >>> let endpoint = Forest.annotate [Description "Get a user", Group "users"] (reqTree Forest.:-> Res.response200)
-- >>> let oa = endpointToOpenApi endpoint
-- >>> let Just pi_ = IHM.lookup "/{userId}" (oa ^. #paths)
-- >>> let Just op = pi_ ^. #get

-- | 'annotate' works at both levels of an endpoint — one wrapping a single
--   path segment (lands on that parameter's nested 'OA.Schema'), one
--   wrapping the whole @req :-> res@ contract (lands on the 'Operation'
--   itself, including OpenAPI's own operation-grouping @tags@ via 'Group'):
--
-- >>> op ^. #description
-- Just "Get a user"
-- >>> op ^. #tags
-- fromList ["users"]
-- >>> let [Inline p] = op ^. #parameters
-- >>> let Just (Inline sc) = p ^. #schema
-- >>> sc ^. #description
-- Just "the user id"
endpointToOpenApi :: Forest (Shape method path query headers body result) -> OpenApi
endpointToOpenApi endpoint = case stripTags endpoint of
    (req :-> singleRes) -> toOpenApi (collectTags endpoint) req [resInfoOf singleRes]
    (req :-< resAlt)    -> toOpenApi (collectTags endpoint) req (extractResInfos resAlt)
    Forest.Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"
  where
    toOpenApi tags req resInfos =
        let
            stdMeth = fromMaybe HTTP.GET (methodStdOf req.method)
            pieces  = walkPath [] req.path
            qParams = extractQueryParams [] req.query
            hParams = extractReqHeaderParams [] req.headers
            reqBody =
                if stdMeth `notElem` [HTTP.GET, HTTP.HEAD]
                    then extractReqBodySchema req.body
                    else Nothing
            allDefs =
                extractReqBodyDefs req.body
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
                    & applyReqBodySchema (extractReqContentType req.headers) reqBody
                    & applyTagsToOperation tags
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

instance GenericOAPI (S1 sm (Rec0 (Forest (Shape method path query headers body result)))) where
    gOpenApi (M1 (K1 ep)) = endpointToOpenApi ep

openApi ::
    forall server.
    ( Generic (server Forest)
    , GenericOAPI (Rep (server Forest))
    ) =>
    server Forest ->
    OpenApi
openApi = gOpenApi @(Rep (server Forest)) . from
