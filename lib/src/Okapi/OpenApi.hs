{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.OpenApi (contractToOpenApi, GOpenApi, openApi, GOpenApiVia, openApiVia) where

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
import Network.HTTP.Types qualified as Types
import Okapi.Contract (Contract ((:->), (:-<)), Signature, stripTags, collectTags, Morph (..))
import Okapi.Contract qualified as Contract
import Okapi.Tree (Info (..), Leaf (..), Tag (..))
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Method qualified as Method
import Okapi.HTTP.Path (Path)
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Query (Query)
import Okapi.HTTP.Query qualified as Query
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Status qualified as Status
import Okapi.HTTP.Responses (Responses, Responses')
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

-- | Fold 'Tag's onto an 'Operation' — the contract-level (whole contract)
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
--   'Okapi.Contract.collectTags').
walkPath :: [Tag] -> Tree Path i o -> [PathPiece]
walkPath _    (Node (Path.Seg_ vLeaf x)) = [PLit (vLeaf.encode x)]
walkPath tags (Node (Path.Seg n vLeaf))  = [PParam n (applyTagsToSchema tags (infoSchema vLeaf.info))]
walkPath tags (Node (Path.Segs vLeaf))   = [PParam "segs" (applyTagsToSchema tags (infoSchema vLeaf.info))]
walkPath _    (Node Path.Base)            = []
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
    Query.Flag   key         -> [mkParam key ParamQuery True]
    Query.Flag'  key         -> [mkParam key ParamQuery False]
    Query.List  style key _  -> [mkArrayParam key True  style]
    Query.List' style key _  -> [mkArrayParam key False style]
    Query.Base                -> []
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

-- | Core traversal shared by both sides — extracts @(name, required?,
--   schema)@ triples for every header-shaped node. 'Headers.Cookie'\/
--   'Headers.Cookie''\/'Headers.SetCookie' have no OpenAPI header
--   representation (cookies are documented separately, if at all), so
--   they contribute nothing here.
extractHeaderInfo :: [Tag] -> Tree (Headers.Headers ctx) i o -> [(Text, Bool, OA.Schema)]
extractHeaderInfo tags (Node hdr) = map applyTags $ case hdr of
    Headers.Field  key vLeaf     -> [(hdrName key, True,  infoSchema vLeaf.info)]
    Headers.Field' key vLeaf     -> [(hdrName key, False, infoSchema vLeaf.info)]
    Headers.Base                  -> []
    Headers.Field_ _ _           -> []
    Headers.FieldStructured n _  -> [(hdrName n, True, mempty & #type ?~ OA.OpenApiString)]
    Headers.Cookie  _ _          -> []
    Headers.Cookie' _ _          -> []
    Headers.SetCookie _ _ _      -> []
  where
    applyTags (name, req_, sc) = (name, req_, applyTagsToSchema tags sc)
extractHeaderInfo tags (FMap _ c)      = extractHeaderInfo tags c
extractHeaderInfo tags (LMap _ c)      = extractHeaderInfo tags c
extractHeaderInfo tags (Apply cf cx)   = extractHeaderInfo tags cf ++ extractHeaderInfo tags cx
extractHeaderInfo _    (Pure _)        = []
extractHeaderInfo tags (Annotate ts c) = extractHeaderInfo (tags <> ts) c

extractContentType :: Tree (Headers.Headers ctx) i o -> Maybe ByteString
extractContentType (Node (Headers.Field_ k v)) | k == "content-type" = Just v
extractContentType (FMap _ c)    = extractContentType c
extractContentType (LMap _ c)    = extractContentType c
extractContentType (Apply cf cx) = extractContentType cf <|> extractContentType cx
extractContentType (Annotate _ c) = extractContentType c
extractContentType _             = Nothing

bodySchemaOf :: forall (b :: Type -> Type) a. Body.IsoJson a => b (IO a) -> OA.Schema
bodySchemaOf _ = toSchema (Proxy @a)

bodyDefsOf :: forall (b :: Type -> Type) a. Body.IsoJson a => b (IO a) -> OA.Definitions OA.Schema
bodyDefsOf _ = execDeclare (declareSchemaRef (Proxy @a)) mempty

extractBodySchema :: Body.Body ctx a -> Maybe OA.Schema
extractBodySchema b = case b of
    Body.Json -> Just (bodySchemaOf b)
    _         -> Nothing

extractBodyDefs :: Body.Body ctx a -> OA.Definitions OA.Schema
extractBodyDefs b = case b of
    Body.Json -> bodyDefsOf b
    _         -> mempty

data ResInfo = ResInfo
    { resStatus     :: Types.Status
    , resMediaType  :: Maybe ByteString
    , resBodySchema :: Maybe OA.Schema
    , resBodyDefs   :: OA.Definitions OA.Schema
    , resHdrNames   :: [(Text, Bool, OA.Schema)]
    }

resStatusOf :: Status.Status s -> Types.Status
resStatusOf Status.Base        = Types.status200
resStatusOf (Status.Status ks) = Status.knownStatusToHTTP ks

resInfoOf :: Res.Codec s h b -> ResInfo
resInfoOf res = ResInfo
    { resStatus     = resStatusOf res.status
    , resMediaType  = extractContentType res.headers
    , resBodySchema = extractBodySchema res.body
    , resBodyDefs   = extractBodyDefs res.body
    , resHdrNames   = extractHeaderInfo [] res.headers
    }

extractResInfos :: Responses responses => Responses' Res.Codec responses -> [ResInfo]
extractResInfos rs =
    map
        (getConst . Resps.traverseResponses @Res.Codec @Res.Codec (\c -> Const (resInfoOf c)))
        (NE.toList (Resps.getResponses rs))

methodStdOf :: Method.Method m -> Maybe Types.StdMethod
methodStdOf Method.Base        = Nothing
methodStdOf (Method.Method km) = Just (Method.knownMethodToStd km)

hdrName :: Types.HeaderName -> Text
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
        & #description .~ T.pack (show (Types.statusCode (resStatus ri)))
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

setMethod :: Types.StdMethod -> Operation -> PathItem -> PathItem
setMethod Types.GET    op pi_ = pi_{_pathItemGet    = Just op}
setMethod Types.POST   op pi_ = pi_{_pathItemPost   = Just op}
setMethod Types.PUT    op pi_ = pi_{_pathItemPut    = Just op}
setMethod Types.DELETE op pi_ = pi_{_pathItemDelete = Just op}
setMethod _           op pi_ = pi_{_pathItemGet    = Just op}

-- $setup
-- >>> :set -XOverloadedLabels
-- >>> import Okapi.Contract qualified as Contract
-- >>> import Okapi.HTTP.Request qualified as Req
-- >>> import Okapi.HTTP.Path (Path)
-- >>> import Okapi.HTTP.Path qualified as Path
-- >>> import Okapi.HTTP.Query qualified as Query
-- >>> import Okapi.HTTP.Method qualified as Method
-- >>> import Okapi.HTTP.Headers qualified as Headers
-- >>> import Okapi.HTTP.Body qualified as Body
-- >>> import Okapi.HTTP.Response qualified as Res
-- >>> import Okapi.Tree (Leaf, Tag (..), annotate, integer)
-- >>> import Data.HashMap.Strict.InsOrd qualified as IHM
-- >>> import Data.OpenApi (Referenced (..))
-- >>> import Optics.Core ((^.))
-- >>> let reqTree = Req.Codec { Req.method = Method.method Method.Get, Req.path = annotate [Description "the user id"] (Path.seg "userId" (integer :: Leaf Path Integer)), Req.query = Query.base, Req.headers = Headers.base, Req.body = Body.base }
-- >>> let contract = Contract.annotate [Description "Get a user", Group "users"] (reqTree Contract.:-> Res.ok)
-- >>> let oa = contractToOpenApi contract
-- >>> let Just pi_ = IHM.lookup "/{userId}" (oa ^. #paths)
-- >>> let Just op = pi_ ^. #get

-- | 'annotate' works at both levels of a contract — one wrapping a single
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
contractToOpenApi :: Contract shape -> OpenApi
contractToOpenApi contract = case stripTags contract of
    (req :-> singleRes) -> toOpenApi (collectTags contract) req [resInfoOf singleRes]
    (req :-< resAlt)    -> toOpenApi (collectTags contract) req (extractResInfos resAlt)
    Contract.Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"
  where
    toOpenApi tags req resInfos =
        let
            stdMeth = fromMaybe Types.GET (methodStdOf req.method)
            pieces  = walkPath [] req.path
            qParams = extractQueryParams [] req.query
            hParams =
                [ mkParamWithSchema name ParamHeader req_ sc
                | (name, req_, sc) <- extractHeaderInfo [] req.headers
                ]
            reqBody =
                if stdMeth `notElem` [Types.GET, Types.HEAD]
                    then extractBodySchema req.body
                    else Nothing
            allDefs =
                extractBodyDefs req.body
                    <> foldMap resBodyDefs resInfos
            op =
                mempty
                    & #parameters .~ map Inline (pathOAParams pieces ++ qParams ++ hParams)
                    & #responses .~ OA.Responses Nothing
                        ( IHM.fromList
                            [ (Types.statusCode (resStatus ri), Inline (mkResResponse ri))
                            | ri <- resInfos
                            ]
                        )
                    & applyReqBodySchema (extractContentType req.headers) reqBody
                    & applyTagsToOperation tags
         in
            mempty
                & #info % #title .~ "API"
                & #info % #version .~ "0.1.0"
                & #components % #schemas .~ allDefs
                & #paths .~ IHM.singleton (pathTemplate pieces) (setMethod stdMeth op mempty)

class GOpenApi (ctF :: Type -> Type) where
    gOpenApi :: ctF () -> OpenApi

instance GOpenApi ctF => GOpenApi (D1 dm ctF) where
    gOpenApi (M1 ct) = gOpenApi @ctF ct

instance GOpenApi ctF => GOpenApi (C1 cm ctF) where
    gOpenApi (M1 ct) = gOpenApi @ctF ct

instance (GOpenApi ctL, GOpenApi ctR) => GOpenApi (ctL :*: ctR) where
    gOpenApi (ctL :*: ctR) = gOpenApi @ctL ctL <> gOpenApi @ctR ctR

instance GOpenApi (S1 sm (Rec0 (Contract (Signature method path query headers body result)))) where
    gOpenApi (M1 (K1 ct)) = contractToOpenApi ct

-- | Lets a field be a nested record of contracts instead of a concrete
--   'Contract' — recurses via 'openApi' itself. Same non-overlap argument
--   as the nested instances in "Okapi.Server".
instance
    ( Generic (nested Contract)
    , GOpenApi (Rep (nested Contract))
    ) =>
    GOpenApi (S1 sm (Rec0 (nested Contract)))
    where
    gOpenApi (M1 (K1 ctVal)) = openApi ctVal

openApi ::
    forall record.
    ( Generic (record Contract)
    , GOpenApi (Rep (record Contract))
    ) =>
    record Contract ->
    OpenApi
openApi = gOpenApi @(Rep (record Contract)) . from

class GOpenApiVia (ctF :: Type -> Type) where
    gOpenApiVia :: ctF () -> OpenApi

instance GOpenApiVia ctF => GOpenApiVia (D1 dm ctF) where
    gOpenApiVia (M1 ct) = gOpenApiVia @ctF ct

instance GOpenApiVia ctF => GOpenApiVia (C1 cm ctF) where
    gOpenApiVia (M1 ct) = gOpenApiVia @ctF ct

instance (GOpenApiVia ctL, GOpenApiVia ctR) => GOpenApiVia (ctL :*: ctR) where
    gOpenApiVia (ctL :*: ctR) = gOpenApiVia @ctL ctL <> gOpenApiVia @ctR ctR

instance GOpenApiVia (S1 sm (Rec0 (Morph Contract n (Signature method path query headers body result)))) where
    gOpenApiVia (M1 (K1 (Morph ct))) = contractToOpenApi ct

-- | Lets a field be a nested record of contracts instead of a concrete
--   @Morph Contract@ — recurses via 'openApiVia' itself.
instance
    ( Generic (nested (Morph Contract))
    , GOpenApiVia (Rep (nested (Morph Contract)))
    ) =>
    GOpenApiVia (S1 sm (Rec0 (nested (Morph Contract))))
    where
    gOpenApiVia (M1 (K1 ctVal)) = openApiVia ctVal

-- | Heterogeneous-@n@ counterpart to 'openApi' — takes a record built with
--   'Okapi.Contract.Morph' (see 'Okapi.Server.serversVia')
--   instead of a plain @record Contract@.
openApiVia ::
    forall record.
    ( Generic (record (Morph Contract))
    , GOpenApiVia (Rep (record (Morph Contract)))
    ) =>
    record (Morph Contract) ->
    OpenApi
openApiVia = gOpenApiVia @(Rep (record (Morph Contract))) . from
