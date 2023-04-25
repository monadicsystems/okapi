{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Endpoint
  ( StdMethod (..),
    Plan (..),
    Endpoint (..),
    Handler,
    Artifact,
    Server (..),
    build,
    status200,
    status404,
    toOpenApi,
    toApp,
  )
where

import Control.Natural (type (~>))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List (groupBy)
import Data.List qualified as List
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
import Extra qualified
import Network.HTTP.Types (StdMethod (..), status200, status404)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Okapi.Request (Request)
import Okapi.Script
import Okapi.Script.AddHeader (Response, toWaiResponse)
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Okapi.Script.Security qualified as Security

data Endpoint s p q b h r = Endpoint
  { security :: Security.Script s,
    method :: HTTP.StdMethod,
    path :: Path.Script p,
    query :: Query.Script q,
    body :: Body.Script b,
    headers :: Headers.Script h,
    responder :: Responder.Script r
  }

toPathItem :: Endpoint s p q b h r -> (FilePath, OAPI.PathItem)
toPathItem endpoint = (pathName, pathItem)
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
        { OAPI._operationParameters = parameters
        }

    parameters :: [OAPI.Referenced OAPI.Param]
    parameters = pathParams endpoint.path <> queryParams endpoint.query <> headersParams endpoint.headers

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

type Handler m s p q b h r = Monad m => s -> p -> q -> b -> h -> r -> m Response

data Plan m s p q h b r = Plan
  { endpoint :: Endpoint s p q b h r,
    handler :: Handler m s p q b h r
  }

data Executable = Run (IO WAI.Response) | Null

type Compiler = Request -> Executable

data Artifact = Artifact
  { compiler :: Compiler,
    pathItem :: (FilePath, OAPI.PathItem)
  }

-- buildWith ::
--   forall m s p q b h r.
--   Monad m =>
--   (m ~> IO) ->
--   Endpoint s p q b h r ->
--   Handler m s p q b h r ->
--   Artifact
-- buildWith transformer endpoint handler = build transformer (Plan {endpoint, handler})

build ::
  forall m s p q h b r.
  Monad m =>
  (m ~> IO) ->
  Plan m s p q h b r ->
  Artifact
build transformer plan = Artifact {..}
  where
    pathItem = toPathItem plan.endpoint
    compiler (method, path, query, body, headers) =
      if method == plan.endpoint.method
        then
          let securityResult = fst $ Security.eval plan.endpoint.security $ Security.State query headers []
              pathResult = fst $ Path.eval plan.endpoint.path path
              queryResult = fst $ Query.eval plan.endpoint.query query
              bodyResult = fst $ Body.eval plan.endpoint.body body
              headersResult = fst $ Headers.eval plan.endpoint.headers headers
              responderResult = fst $ Responder.eval plan.endpoint.responder ()
           in case (securityResult, pathResult, queryResult, bodyResult, headersResult, responderResult) of
                (Ok s, Ok p, Ok q, Ok b, Ok h, Ok r) -> Run do
                  response <- transformer $ handler plan s p q b h r
                  return $ toWaiResponse response
                _ -> Null
        else Null

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
      if Extra.dropPrefix server.url path == path
        then do
          respond server.defaultResponse
        else do
          body <- WAI.strictRequestBody waiRequest
          let Right method = HTTP.parseMethod $ WAI.requestMethod waiRequest
              -- \^ TODO: Fix above. Not complete
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

-- groupBy (\(fp, _) (fp', _) -> fp == fp')
-- merge :: OAPI.PathItem -> OAPI.PathItem -> OAPI.PathItem
-- merge pathItem1 pathItem2 =
--   pathItem1
--     { OAPI._pathItemGet = help pathItem1._pathItemGet pathItem2._pathItemGet,
--       OAPI._pathItemPost = help pathItem1._pathItemPost pathItem2._pathItemPost,
--       OAPI._pathItemPut = help pathItem1._pathItemPut pathItem2._pathItemPut,
--       OAPI._pathItemDelete = help pathItem1._pathItemDelete pathItem2._pathItemDelete,
--       OAPI._pathItemOptions = help pathItem1._pathItemOptions pathItem2._pathItemOptions,
--       OAPI._pathItemHead = help pathItem1._pathItemHead pathItem2._pathItemHead,
--       OAPI._pathItemPatch = help pathItem1._pathItemPatch pathItem2._pathItemPatch,
--       OAPI._pathItemTrace = help pathItem1._pathItemTrace pathItem2._pathItemTrace
--     }
--   where
--     help Nothing Nothing = Nothing
--     help x@(Just _) y@(Just _) = x <> y
--     help x@(Just _) Nothing = x
--     help Nothing y@(Just _) = y

(|>) = (&)

toJSClient ::
  Server ->
  BS.ByteString
toJSClient = undefined
