{-# LANGUAGE BlockArguments #-}
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
import Control.Object (Object (..), (#))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
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

data PathItem resource where
  PathItem ::
    forall resource m1 m2 m3 m4.
    (Monad m1, Monad m2, Monad m3, Monad m4) =>
    { path :: Path.Script resource,
      get :: Maybe (GET resource m1),
      post :: Maybe (POST resource m2),
      put :: Maybe (PUT resource m3),
      delete :: Maybe (DELETE resource m4),
      summary :: Maybe Text.Text,
      description :: Maybe Text.Text
    } ->
    PathItem resource

data GET resource m where
  GET ::
    Monad m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      responder :: Responder.Script r,
      handler :: s -> resource -> q -> h -> r %1 -> m Response,
      object :: Object m
    } ->
    GET resource m

data POST resource m where
  POST ::
    Monad m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      body :: NonEmpty (Body.Script b),
      responder :: Responder.Script r,
      handler :: s -> resource -> q -> b -> h -> r %1 -> m Response,
      object :: Object m
    } ->
    POST resource m

data PUT resource m where
  PUT ::
    Monad m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      body :: NonEmpty (Body.Script b),
      responder :: Responder.Script r,
      handler :: s -> resource -> q -> b -> h -> r %1 -> m Response,
      object :: Object m
    } ->
    PUT resource m

data DELETE resource m where
  DELETE ::
    Monad m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      security :: NonEmpty (Security.Script s),
      query :: Query.Script q,
      headers :: Headers.Script h,
      responder :: Responder.Script r,
      handler :: s -> resource -> q -> h -> r %1 -> m Response,
      object :: Object m
    } ->
    DELETE resource m

data Server resource = Server
  { info :: OAPI.Info,
    url :: [Text.Text],
    description :: Maybe Text.Text,
    pathItems :: [PathItem resource]
  }

toApplication :: Server resource -> WAI.Application
toApplication Server {url, pathItems} request respond = do
  let reqPath = List.dropPrefix url $ WAI.pathInfo request
      reqQuery = WAI.queryString request
      reqHeaders = WAI.requestHeaders request
      reqSecurity = Secure.State reqQuery reqHeaders [] -- TODO: Get cookies out of headers here instead of empty list
  if reqPath == WAI.pathInfo request && not (null url)
    then respond $ WAI.responseLBS HTTP.status404 mempty mempty -- TODO: If the incoming request URL doesn't have correct prefix. 404?
    else case getPathItemByPath reqPath pathItems of
      Nothing -> respond $ WAI.responseLBS HTTP.status404 mempty mempty
      Just (PathItem {get, post, put, delete}, resourceParam) -> case HTTP.parseMethod $ WAI.requestMethod request of
        Left _ -> respond $ WAI.responseLBS HTTP.status501 mempty mempty -- TODO: Return 501 Not Implemented error
        Right reqMethod ->
          case reqMethod of
            HTTP.GET -> case get of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (GET {security, query, headers, responder, handler, object}) ->
                case evalSecurity (sortSecurity security) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval query reqQuery, Headers.eval headers reqHeaders, Responder.eval responder ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      response <- object # handler securityParam resourceParam queryParam headersParam responderParam
                      respond $ toWaiResponse response
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            HTTP.POST -> case post of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (POST {security, query, body, headers, responder, handler, object}) ->
                case evalSecurity (sortSecurity security) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval query reqQuery, Headers.eval headers reqHeaders, Responder.eval responder ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      bodyResult <- evalBody (sortBody body) request
                      case bodyResult of
                        (Ok bodyParam, _) -> do
                          response <- object # handler securityParam resourceParam queryParam bodyParam headersParam responderParam
                          respond $ toWaiResponse response
                        _ -> respond $ WAI.responseLBS HTTP.status400 mempty mempty -- TODO: Return 400 for now but can be more specific depending on content-type, etc.
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            HTTP.PUT -> case put of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (PUT {security, query, body, headers, responder, handler, object}) ->
                case evalSecurity (sortSecurity security) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval query reqQuery, Headers.eval headers reqHeaders, Responder.eval responder ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      bodyResult <- evalBody (sortBody body) request
                      case bodyResult of
                        (Ok bodyParam, _) -> do
                          response <- object # handler securityParam resourceParam queryParam bodyParam headersParam responderParam
                          respond $ toWaiResponse response
                        _ -> respond $ WAI.responseLBS HTTP.status400 mempty mempty -- TODO: Return 400 for now but can be more specific depending on content-type, etc.
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            HTTP.DELETE -> case delete of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (DELETE {security, query, headers, responder, handler, object}) ->
                case evalSecurity (sortSecurity security) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval query reqQuery, Headers.eval headers reqHeaders, Responder.eval responder ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      response <- object # handler securityParam resourceParam queryParam headersParam responderParam
                      respond $ toWaiResponse response
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            _ -> respond $ WAI.responseLBS HTTP.status501 mempty mempty -- TODO: Implement cases for remaing Standard HTTP methods

getPathItemByPath :: [Text.Text] -> [PathItem resource] -> Maybe (PathItem resource, resource)
getPathItemByPath reqPath [] = Nothing
getPathItemByPath reqPath (pathItem@PathItem {path} : t) = case Path.eval path reqPath of
  (Ok resourceParam, _) -> Just (pathItem, resourceParam)
  _ -> getPathItemByPath reqPath t

evalBody :: NonEmpty (Body.Script a) -> WAI.Request -> IO (Result [Body.Error] a, Body.RequestBody)
evalBody (h :| t) request = do
  state <- case WAI.getRequestBodyType request of
    Just (WAI.Multipart _boundary) -> Body.RequestBodyMultipart <$> WAI.parseRequestBodyEx WAI.defaultParseRequestBodyOptions WAI.lbsBackEnd request
    _ -> Body.RequestBodyRaw <$> WAI.lazyRequestBody request
  case first (first pure) $ Body.eval h state of
    (ok@(Ok _), s) -> return (ok, s)
    _ -> return $ loop state t
  where
    loop :: Body.RequestBody -> [Body.Script a] -> (Result [Body.Error] a, Body.RequestBody)
    loop state [] = (Fail [], state)
    loop state (h : t) = case first (first pure) $ Body.eval h state of
      (ok@(Ok _), state') -> (ok, state')
      _ -> loop state t

sortBody :: NonEmpty (Body.Script a) -> NonEmpty (Body.Script a)
sortBody = NonEmpty.sortBy comparer
  where
    comparer :: Body.Script a -> Body.Script a -> Ordering
    comparer Body.None Body.None = EQ
    comparer (Body.FMap _ Body.None) (Body.FMap _ Body.None) = EQ
    comparer (Body.FMap _ Body.None) _ = GT
    comparer _ (Body.FMap _ Body.None) = LT
    comparer Body.None _body = GT
    comparer _body Body.None = LT

evalSecurity :: NonEmpty (Security.Script a) -> Secure.State -> (Result Security.Error a, Secure.State)
evalSecurity (h :| t) state = case Security.eval h state of
  (ok@(Ok _), s) -> (ok, s)
  _ -> loop state t
  where
    loop :: Secure.State -> [Security.Script a] -> (Result Security.Error a, Secure.State)
    loop state [] = (Fail $ Security.SecureError Secure.ParseFail, state)
    loop state (h : t) = case Security.eval h state of
      (ok@(Ok _), state') -> (ok, state')
      _ -> loop state t

sortSecurity :: NonEmpty (Security.Script a) -> NonEmpty (Security.Script a)
sortSecurity = NonEmpty.sortBy comparer
  where
    comparer :: Security.Script a -> Security.Script a -> Ordering
    comparer Security.None Security.None = EQ
    comparer (Security.FMap _ Security.None) (Security.FMap _ Security.None) = EQ
    comparer Security.None _ = GT
    comparer (Security.FMap _ Security.None) _ = GT
    comparer _ Security.None = LT
    comparer _ (Security.FMap _ Security.None) = LT

toOpenApi ::
  Server resource ->
  OAPI.OpenApi
toOpenApi Server {info, description, pathItems, url} =
  mempty
    { OAPI._openApiInfo = info,
      OAPI._openApiServers =
        [ OAPI.Server
            (Text.intercalate "/" url)
            description
            mempty
        ],
      OAPI._openApiPaths = InsOrdHashMap.fromList $ map toOpenAPIPathItem pathItems,
      OAPI._openApiOpenapi = OpenApiSpecVersion {getVersion = Version.Version [3, 0, 3] []}
    }

toOpenAPIPathItem :: PathItem resource -> (FilePath, OAPI.PathItem)
toOpenAPIPathItem PathItem {path, get, post, put, delete, summary, description} = (pathName, pathItem)
  where
    pathName :: FilePath
    pathName = renderPath path

    pathItem :: OAPI.PathItem
    pathItem =
      mempty
        { OAPI._pathItemSummary = summary,
          OAPI._pathItemDescription = description,
          OAPI._pathItemGet = fmap (toGetOperation path) get,
          OAPI._pathItemPost = fmap (toPostOperation path) post,
          OAPI._pathItemPut = fmap (toPutOperation path) put,
          OAPI._pathItemDelete = fmap (toDeleteOperation path) delete
        }

toGetOperation :: Path.Script resource -> GET resource m -> OAPI.Operation
toGetOperation path GET {security, query, headers, responder, description, summary} =
  mempty
    { OAPI._operationParameters = toParameters (path, query, headers),
      OAPI._operationResponses = toResponses responder,
      OAPI._operationSecurity = toSecurityRequirements security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toPostOperation :: Path.Script resource -> POST resource m -> OAPI.Operation
toPostOperation path POST {security, query, body, headers, responder, description, summary} =
  mempty
    { OAPI._operationParameters = toParameters (path, query, headers),
      OAPI._operationRequestBody = toOpenAPIRequestBody body,
      OAPI._operationResponses = toResponses responder,
      OAPI._operationSecurity = toSecurityRequirements security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toPutOperation :: Path.Script resource -> PUT resource m -> OAPI.Operation
toPutOperation path PUT {security, query, body, headers, responder, summary, description} =
  mempty
    { OAPI._operationParameters = toParameters (path, query, headers),
      OAPI._operationRequestBody = toOpenAPIRequestBody body,
      OAPI._operationResponses = toResponses responder,
      OAPI._operationSecurity = toSecurityRequirements security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toDeleteOperation :: Path.Script resource -> DELETE resource m -> OAPI.Operation
toDeleteOperation path DELETE {security, query, headers, responder, summary, description} =
  mempty
    { OAPI._operationParameters = toParameters (path, query, headers),
      OAPI._operationResponses = toResponses responder,
      OAPI._operationSecurity = toSecurityRequirements security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toParameters :: (Path.Script resource, Query.Script q, Headers.Script h) -> [OAPI.Referenced OAPI.Param]
toParameters (path, query, headers) = pathParameters path <> queryParameters query <> headersParameters headers
  where
    pathParameters :: Path.Script resource -> [OAPI.Referenced OAPI.Param]
    pathParameters path = case path of
      Path.FMap f p -> pathParameters p
      Path.Pure _ -> mempty
      Path.Apply pf px -> pathParameters pf <> pathParameters px
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

    queryParameters :: Query.Script q -> [OAPI.Referenced OAPI.Param]
    queryParameters query = case query of
      Query.FMap f q -> queryParameters q
      Query.Pure _ -> mempty
      Query.Apply pf px -> queryParameters pf <> queryParameters px
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
          param <- queryParameters query'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        Query.Flag _ -> do
          param <- queryParameters query'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        _ -> queryParameters query'
      Query.Option @p def query' -> case query' of
        Query.Param _ -> do
          param <- queryParameters query'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False, OAPI._paramSchema = fmap (fmap (\schema -> schema {OAPI._schemaDefault = Just $ Aeson.toJSON def})) param._paramSchema}) param
        _ -> queryParameters query'

    headersParameters :: Headers.Script h -> [OAPI.Referenced OAPI.Param]
    headersParameters headers = case headers of
      Headers.FMap f h -> headersParameters h
      Headers.Pure _ -> mempty
      Headers.Apply pf px -> headersParameters pf <> headersParameters px
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
          param <- headersParameters headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        Headers.Cookie _ -> do
          param <- headersParameters headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False}) param
        _ -> headersParameters headers'
      Headers.Option @p def headers' -> case headers' of
        Headers.Param _ -> do
          param <- headersParameters headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False, OAPI._paramSchema = fmap (fmap (\schema -> schema {OAPI._schemaDefault = Just $ Aeson.toJSON def})) param._paramSchema}) param
        Headers.Cookie _ -> do
          param <- headersParameters headers'
          pure $ fmap (\param -> param {OAPI._paramRequired = Just False, OAPI._paramSchema = fmap (fmap (\schema -> schema {OAPI._schemaDefault = Just $ Aeson.toJSON def})) param._paramSchema}) param
        _ -> headersParameters headers'

toSecurityRequirements :: NonEmpty (Security.Script s) -> [OAPI.SecurityRequirement]
toSecurityRequirements security = []

toOpenAPIRequestBody :: NonEmpty (Body.Script b) -> Maybe (OAPI.Referenced OAPI.RequestBody)
toOpenAPIRequestBody body = Nothing

toResponses :: Responder.Script r -> OAPI.Responses
toResponses responder = mempty

renderPath :: Path.Script a -> FilePath
renderPath path = case path of
  Path.FMap f p -> renderPath p
  Path.Pure _ -> mempty
  Path.Apply pf px -> renderPath pf <> renderPath px
  Path.Static t -> "/" <> Text.unpack t
  Path.Param @p name -> "/{" <> Text.unpack name <> "}"
