{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Operation where

-- ( StdMethod (..),
--   Plan (..),
--   Operation (..),
--   Handler,
--   Artifact,
--   Server (..),
--   build,
--   status200,
--   status404,
--   toOpenApi,
--   toApp,
-- )

import Control.Natural (type (~>))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List (groupBy)
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.OpenApi (OpenApi (_openApiInfo))
import Data.OpenApi qualified as OAPI
import Data.OpenApi.Declare qualified as OAPI
import Data.OpenApi.Internal (OpenApiSpecVersion (..), upperOpenApiSpecVersion)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Version qualified as Version
import Debug.Trace qualified as Debug
-- import Network.HTTP.Types (StdMethod (..), status200, status404)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Network.Wai.Parse qualified as WAI
import Okapi.Request (Request)
import Okapi.Script
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Okapi.Script.Responder.AddHeader (Response, toWaiResponse)
import Okapi.Script.Security qualified as Security
import Okapi.Script.Security.Secure qualified as Secure

data PathItem p where
  PathItem ::
    (Monad a, Monad b, Monad c, Monad d) =>
    { path :: Path.Script p,
      get :: Maybe (GET a p, a ~> IO),
      post :: Maybe (POST b p, b ~> IO),
      put :: Maybe (PUT c p, c ~> IO),
      delete :: Maybe (DELETE d p, d ~> IO),
      summary :: Text.Text,
      description :: Text.Text
    } ->
    PathItem p

data GET m p where
  GET ::
    Monad m =>
    { security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      responder :: Responder.Script r,
      handler :: s -> p -> q -> h -> r %1 -> m Response
    } ->
    GET m p

data POST m p where
  POST ::
    Monad m =>
    { security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      body :: NonEmpty (Body.Script b),
      responder :: Responder.Script r,
      handler :: s -> p -> q -> b -> h -> r %1 -> m Response
    } ->
    POST m p

data PUT m p where
  PUT ::
    Monad m =>
    { security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      body :: NonEmpty (Body.Script b),
      responder :: Responder.Script r,
      handler :: s -> p -> q -> b -> h -> r %1 -> m Response
    } ->
    PUT m p

data DELETE m p where
  DELETE ::
    Monad m =>
    { security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      responder :: Responder.Script r,
      handler :: s -> p -> q -> h -> r %1 -> m Response
    } ->
    DELETE m p

data Executable = Run (IO WAI.Response) | Null

type Compiler = Request -> Executable

data Artifact = Artifact
  { compiler :: Compiler,
    pathItem :: (FilePath, OAPI.PathItem)
  }

build ::
  PathItem p ->
  Artifact
build path = Artifact {..}
  where
    pathItem = toPathItem path
    compiler = toCompiler path

evalSecurities :: Secure.State -> NonEmpty (Security.Script a) -> (Result Security.Error a, Secure.State)
evalSecurities state (h :| t) = case Security.eval h state of
  (ok@(Ok _), s) -> (ok, s)
  _ -> loop state t
  where
    loop :: Secure.State -> [Security.Script a] -> (Result Security.Error a, Secure.State)
    loop state [] = (Fail $ Security.SecureError Secure.ParseFail, state)
    loop state (h : t) = case Security.eval h state of
      (ok@(Ok _), state') -> (ok, state')
      _ -> loop state t

moveSecurityNonesToEnd :: NonEmpty (Security.Script a) -> NonEmpty (Security.Script a)
moveSecurityNonesToEnd = NonEmpty.sortBy comparer
  where
    comparer :: Security.Script a -> Security.Script a -> Ordering
    comparer Security.None Security.None = EQ
    comparer (Security.FMap _ Security.None) (Security.FMap _ Security.None) = EQ
    comparer Security.None _ = GT
    comparer (Security.FMap _ Security.None) _ = GT
    comparer _ Security.None = LT
    comparer _ (Security.FMap _ Security.None) = LT

toCompiler :: PathItem p -> Request -> Executable
toCompiler PathItem {..} (reqMethod, reqPath, reqQuery, reqBody, reqHeaders) =
  let reqSecurity = Secure.State reqQuery reqHeaders [] -- TODO: Get cookies out of headers here instead of empty list
   in case Path.eval path reqPath of
        (Ok pathParam, _) -> case reqMethod of
          HTTP.GET -> case get of
            Nothing -> Null
            Just (GET {..}, transformer) ->
              case (fst $ evalSecurities reqSecurity $ moveSecurityNonesToEnd security, fst $ Query.eval query reqQuery, fst $ Headers.eval headers reqHeaders, fst $ Responder.eval responder ()) of
                (Ok securityParam, Ok queryParam, Ok headersParam, Ok responderParam) -> Run do
                  response <- transformer $ handler securityParam pathParam queryParam headersParam responderParam
                  return $ toWaiResponse response
                _ -> Null
          HTTP.POST -> case post of
            Nothing -> undefined
            Just (operation, transformer) -> undefined
          HTTP.PUT -> case put of
            Nothing -> undefined
            Just (operation, transformer) -> undefined
          HTTP.DELETE -> case delete of
            Nothing -> undefined
            Just (operation, transformer) -> undefined
          _ -> Null
        _ -> Null

{-
  if method == plan.endpoint.method
    then
      let securityResult = fst $ Security.eval plan.endpoint.security $ Security.State query headers []
          pathResult = fst $ Path.eval plan.endpoint.path path
          queryResult = fst $ Query.eval plan.endpoint.query query
          bodyResult = Nothing
          -- bodyResult = case plan.endpoint.body of
          --   [] -> Nothing
          --   bodyScripts -> case firstBodyOk $  -- fst $ Body.eval plan.endpoint.body body
          headersResult = fst $ Headers.eval plan.endpoint.headers headers
          responderResult = fst $ Responder.eval plan.endpoint.responder ()
       in case (securityResult, pathResult, queryResult, bodyResult, headersResult, responderResult) of
            (Ok s, Ok p, Ok q, Nothing, Ok h, Ok r) -> Run do
              response <- transformer $ handler plan s p q Nothing h r
              return $ toWaiResponse response
            (Ok s, Ok p, Ok q, Just b, Ok h, Ok r) -> Run do
              response <- transformer $ handler plan s p q (Just b) h r
              return $ toWaiResponse response
            _ -> Null
    else Null
-}

toPathItem :: PathItem p -> (FilePath, OAPI.PathItem)
toPathItem path = undefined

{-
  where
    pathName :: FilePath
    pathName = renderPath endpoint.path

    pathItem :: OAPI.PathItem
    pathItem = addOperationForMethod endpoint.method operation mempty

    addOperationForMethod :: HTTP.StdMethod -> OAPI.Operation -> OAPI.PathItem -> OAPI.PathItem
    addOperationForMethod method operation pathItem = case method of
      HTTP.GET -> pathItem {OAPI._pathItemGet = Just operation}
      HTTP.POST -> pathItem {OAPI._pathItemPost = Just operation}
      HTTP.HEAD -> pathItem {OAPI._pathItemHead = Just operation}
      HTTP.PUT -> pathItem {OAPI._pathItemPut = Just operation}
      HTTP.DELETE -> pathItem {OAPI._pathItemDelete = Just operation}
      HTTP.TRACE -> pathItem {OAPI._pathItemTrace = Just operation}
      HTTP.CONNECT -> pathItem {OAPI._pathItemTrace = Just operation} -- TODO: Not Correct!
      HTTP.OPTIONS -> pathItem {OAPI._pathItemOptions = Just operation}
      HTTP.PATCH -> pathItem {OAPI._pathItemPatch = Just operation}

    operation :: OAPI.Operation
    operation =
      mempty
        { OAPI._operationParameters = parameters,
          OAPI._operationResponses = responses,
          OAPI._operationSecurity = security
        }

    parameters :: [OAPI.Referenced OAPI.Param]
    parameters = pathParams endpoint.path <> queryParams endpoint.query <> headersParams endpoint.headers

    responses :: OAPI.Responses
    responses = undefined

    security :: [OAPI.SecurityRequirement]
    security = undefined

    pathParams :: Path.Script a -> [OAPI.Referenced OAPI.Param]
    pathParams path = case path of
      Path.FMap f p -> pathParams p
      Path.Pure _ -> mempty
      Path.Apply pf px -> pathParams pf <> pathParams px
      Path.Static _ -> mempty
      Path.Param @p name ->
        [ OAPI.Inline $
            mempty
              { OAPI._paramName = name,
                OAPI._paramRequired = Just True,
                OAPI._paramIn = OAPI.ParamPath,
                OAPI._paramSchema = Just $ OAPI.Inline $ OAPI._namedSchemaSchema $ OAPI.undeclare $ OAPI.declareNamedSchema @p Proxy
              }
        ]

    queryParams :: Query.Script a -> [OAPI.Referenced OAPI.Param]
    queryParams path = case path of
      Query.FMap f q -> queryParams q
      Query.Pure _ -> mempty
      Query.Apply pf px -> queryParams pf <> queryParams px
      Query.Param @p name ->
        [ OAPI.Inline $
            mempty
              { OAPI._paramName = Text.decodeUtf8 name,
                OAPI._paramRequired = Just True,
                OAPI._paramIn = OAPI.ParamQuery,
                OAPI._paramSchema = Just $ OAPI.Inline $ OAPI._namedSchemaSchema $ OAPI.undeclare $ OAPI.declareNamedSchema @p Proxy
              }
        ]
      Query.Flag name ->
        [ OAPI.Inline $
            mempty
              { OAPI._paramName = Text.decodeUtf8 name,
                OAPI._paramRequired = Just True,
                OAPI._paramIn = OAPI.ParamQuery,
                OAPI._paramAllowEmptyValue = Just True
              }
        ]
      Query.Optional @p query' -> case query' of
        Query.Param _ -> do
          param <- queryParams query'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        Query.Flag _ -> do
          param <- queryParams query'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        _ -> queryParams query'
      Query.Option @p def query' -> case query' of
        Query.Param _ -> do
          param <- queryParams query'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False, OAPI._paramSchema = fmap (fmap (\schema -> schema {OAPI._schemaDefault = Just $ Aeson.toJSON def})) param._paramSchema}) param
        _ -> queryParams query'

    headersParams :: Headers.Script a -> [OAPI.Referenced OAPI.Param]
    headersParams path = case path of
      Headers.FMap f h -> headersParams h
      Headers.Pure _ -> mempty
      Headers.Apply pf px -> headersParams pf <> headersParams px
      Headers.Param @p name ->
        [ OAPI.Inline $
            mempty
              { OAPI._paramName = Text.decodeUtf8 $ CI.original name,
                OAPI._paramRequired = Just True,
                OAPI._paramIn = OAPI.ParamHeader,
                OAPI._paramSchema = Just $ OAPI.Inline $ OAPI._namedSchemaSchema $ OAPI.undeclare $ OAPI.declareNamedSchema @p Proxy
              }
        ]
      Headers.Cookie @p name ->
        [ OAPI.Inline $
            mempty
              { OAPI._paramName = Text.decodeUtf8 name,
                OAPI._paramRequired = Just True,
                OAPI._paramIn = OAPI.ParamCookie,
                OAPI._paramSchema = Just $ OAPI.Inline $ OAPI._namedSchemaSchema $ OAPI.undeclare $ OAPI.declareNamedSchema @p Proxy
              }
        ]
      Headers.Optional @p headers' -> case headers' of
        Headers.Param _ -> do
          param <- headersParams headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        Headers.Cookie _ -> do
          param <- headersParams headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        _ -> headersParams headers'
      Headers.Option @p def headers' -> case headers' of
        Headers.Param _ -> do
          param <- headersParams headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False, OAPI._paramSchema = fmap (fmap (\schema -> schema {OAPI._schemaDefault = Just $ Aeson.toJSON def})) param._paramSchema}) param
        Headers.Cookie _ -> do
          param <- headersParams headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False, OAPI._paramSchema = fmap (fmap (\schema -> schema {OAPI._schemaDefault = Just $ Aeson.toJSON def})) param._paramSchema}) param
        _ -> headersParams headers'

    renderPath :: Path.Script a -> FilePath
    renderPath path = case path of
      Path.FMap f p -> renderPath p
      Path.Pure _ -> mempty
      Path.Apply pf px -> renderPath pf <> renderPath px
      Path.Static t -> "/" <> Text.unpack t
      Path.Param @p name -> "/{" <> Text.unpack name <> "}"
-}

{-
type Handler m s p q b h r = Monad m => s -> p -> q -> b -> h -> r %1 -> m Response

data Plan m s p q h b r = Plan
  { endpoint :: Operation s p q b h r,
    handler :: Handler m s p q b h r
  }

-- buildWith ::
--   forall m s p q b h r.
--   Monad m =>
--   (m ~> IO) ->
--   Operation s p q b h r ->
--   Handler m s p q b h r ->
--   Artifact
-- buildWith transformer endpoint handler = build transformer (Plan {endpoint, handler})

firstBodyOk :: [(Result Body.Error a, Body.RequestBody)] -> Maybe (Result Body.Error a, Body.RequestBody)
firstBodyOk [] = Nothing
firstBodyOk (h : t) = case h of
  (ok@(Ok _), state) -> Just (ok, state)
  _ -> firstBodyOk t

data Server = Server
  { info :: OAPI.Info,
    url :: [Text.Text],
    description :: Maybe Text.Text,
    artifacts :: [Artifact],
    defaultResponse :: WAI.Response
  }

data Options = Options

toApp ::
  Server ->
  WAI.Application
toApp server = app
  where
    app :: WAI.Application
    app waiRequest respond = do
      let path = WAI.pathInfo waiRequest
      if List.dropPrefix server.url path == path
        then do
          respond server.defaultResponse
        else do
          body <- case WAI.getRequestBodyType waiRequest of
            Just (WAI.Multipart boundary) -> Body.RequestBodyMultipart <$> WAI.parseRequestBodyEx WAI.defaultParseRequestBodyOptions WAI.lbsBackEnd waiRequest
            _ -> Body.RequestBodyRaw <$> WAI.strictRequestBody waiRequest
          let Right method = HTTP.parseMethod $ WAI.requestMethod waiRequest -- TODO: Fix. Not complete
              query = WAI.queryString waiRequest
              headers = WAI.requestHeaders waiRequest
              request = (method, path, query, body, headers)
              executables = map (($ request) . compiler) (artifacts server)
              loop :: [Executable] -> Executable
              loop [] = Null
              loop (h : t) = case h of
                Null -> loop t
                runnable -> runnable
          case loop executables of
            Null -> respond server.defaultResponse
            Run action -> action >>= respond

toOpenApi ::
  Server ->
  OAPI.OpenApi
toOpenApi server =
  mempty
    { OAPI._openApiInfo = server.info,
      OAPI._openApiServers =
        [ OAPI.Server
            (Text.intercalate "/" server.url)
            server.description
            mempty
        ],
      OAPI._openApiPaths = InsOrdHashMap.fromList cleaned,
      OAPI._openApiOpenapi = OpenApiSpecVersion {getVersion = Version.Version [3, 0, 3] []}
    }
  where
    cleaned =
      let clean :: NonEmpty.NonEmpty (FilePath, OAPI.PathItem) -> (FilePath, OAPI.PathItem)
          clean (h@(fp, pi) :| t) = (fp, pi <> loop (map snd t))

          loop :: [OAPI.PathItem] -> OAPI.PathItem
          loop [] = mempty
          loop (h : t) = h <> loop t
       in map pathItem server.artifacts
            |> List.sortBy (\(fp, _) (fp', _) -> compare fp fp')
            |> NonEmpty.groupBy (\(fp, _) (fp', _) -> fp == fp')
            |> fmap clean

(|>) = (&)

toJSClient ::
  Server ->
  BS.ByteString
toJSClient = undefined
-}