{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Server where

import Control.Natural (type (~>))
import Control.Object (Object (..), (#))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Kind (Type)
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
import Okapi.Spec
import Okapi.Spec.Request.Body qualified as Body
import Okapi.Spec.Request.Headers qualified as Headers
import Okapi.Spec.Request.Query qualified as Query
import Okapi.Spec.Response qualified as Response
import Okapi.Spec.Response.Headers (Response, toWaiResponse)
import Okapi.Spec.Request.Path qualified as Path
import Okapi.Spec.Request.Security qualified as Security
-- import Okapi.Spec.Request.Security.Secure qualified as Secure
import Okapi.Spec.Request (Request)
import Okapi.Route (DELETE (..), GET (..), POST (..), PUT (..), Route (..), Routes (..))

data Server routes = Server
  { info :: OAPI.Info,
    url :: [Text.Text],
    description :: Maybe Text.Text,
    routes :: Routes routes
  }

toApplication :: Server routes -> WAI.Application
toApplication Server {url, routes} request respond = do
  let reqPath = List.dropPrefix url $ WAI.pathInfo request
      reqQuery = WAI.queryString request
      reqHeaders = WAI.requestHeaders request
      reqSecurity = Secure.State reqQuery reqHeaders [] -- TODO: Get cookies out of headers here instead of empty list
  if reqPath == WAI.pathInfo request && not (null url)
    then respond $ WAI.responseLBS HTTP.status404 mempty mempty -- TODO: If the incoming request URL doesn't have correct prefix. 404?
    else case getPathItemByPath reqPath routes of
      None -> respond $ WAI.responseLBS HTTP.status404 mempty mempty
      Some (Route {get, post, put, delete}, resourceParam) -> case HTTP.parseMethod $ WAI.requestMethod request of
        Left _ -> respond $ WAI.responseLBS HTTP.status501 mempty mempty -- TODO: Return 501 Not Implemented error
        Right reqMethod ->
          case reqMethod of
            HTTP.GET -> case get of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (GET {handler, object}) ->
                case evalSecurity (sortSecurity Security.parser) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval Query.parser reqQuery, Headers.eval Headers.parser reqHeaders, Response.eval Response.parser ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      response <- object # handler resourceParam securityParam queryParam headersParam responderParam
                      respond $ toWaiResponse response
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            HTTP.POST -> case post of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (POST {handler, object}) ->
                case evalSecurity (sortSecurity Security.parser) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval Query.parser reqQuery, Headers.eval Headers.parser reqHeaders, Response.eval Response.parser ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      bodyResult <- evalBody (sortBody Body.parser) request
                      case bodyResult of
                        (Ok bodyParam, _) -> do
                          response <- object # handler resourceParam securityParam queryParam bodyParam headersParam responderParam
                          respond $ toWaiResponse response
                        _ -> respond $ WAI.responseLBS HTTP.status400 mempty mempty -- TODO: Return 400 for now but can be more specific depending on content-type, etc.
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            HTTP.PUT -> case put of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (PUT {handler, object}) ->
                case evalSecurity (sortSecurity Security.parser) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval Query.parser reqQuery, Headers.eval Headers.parser reqHeaders, Response.eval Response.parser ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      bodyResult <- evalBody (sortBody Body.parser) request
                      case bodyResult of
                        (Ok bodyParam, _) -> do
                          response <- object # handler resourceParam securityParam queryParam bodyParam headersParam responderParam
                          respond $ toWaiResponse response
                        _ -> respond $ WAI.responseLBS HTTP.status400 mempty mempty -- TODO: Return 400 for now but can be more specific depending on content-type, etc.
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            HTTP.DELETE -> case delete of
              Nothing -> respond $ WAI.responseLBS HTTP.status405 mempty mempty
              Just (DELETE {handler, object}) ->
                case evalSecurity (sortSecurity Security.parser) reqSecurity of
                  (Ok securityParam, _) -> case (Query.eval Query.parser reqQuery, Headers.eval Headers.parser reqHeaders, Response.eval Response.parser ()) of
                    ((Ok queryParam, _), (Ok headersParam, _), (Ok responderParam, _)) -> do
                      response <- object # handler resourceParam securityParam queryParam headersParam responderParam
                      respond $ toWaiResponse response
                    _ -> respond $ WAI.responseLBS HTTP.status422 mempty mempty -- TODO: Return 422 Unprocessable Content based on errors returned by Scripts
                  _ -> respond $ WAI.responseLBS HTTP.status401 mempty mempty -- TODO: Return 401 Unauthorized
            _ -> respond $ WAI.responseLBS HTTP.status501 mempty mempty -- TODO: Implement cases for remaing Standard HTTP methods

data Option where
  None :: Option
  Some :: forall route. (Route route, route) -> Option

getPathItemByPath :: [Text.Text] -> Routes routes -> Option
getPathItemByPath reqPath Nil = None
getPathItemByPath reqPath (route@(Route @route _ _ _ _ _ _) :& t) = case Path.eval (Path.parser @route) reqPath of
  (Ok resourceParam, _) -> Some (route, resourceParam)
  _ -> getPathItemByPath reqPath t

evalBody :: NonEmpty (Body.Spec a) -> WAI.Request -> IO (Result [Body.Error] a, Body.RequestBody)
evalBody (h :| t) request = do
  state <- case WAI.getRequestBodyType request of
    Just (WAI.Multipart _boundary) -> Body.RequestBodyMultipart <$> WAI.parseRequestBodyEx WAI.defaultParseRequestBodyOptions WAI.lbsBackEnd request
    _ -> Body.RequestBodyRaw <$> WAI.lazyRequestBody request
  case first (first pure) $ Body.eval h state of
    (ok@(Ok _), s) -> return (ok, s)
    _ -> return $ loop state t
  where
    loop :: Body.RequestBody -> [Body.Spec a] -> (Result [Body.Error] a, Body.RequestBody)
    loop state [] = (Fail [], state)
    loop state (h : t) = case first (first pure) $ Body.eval h state of
      (ok@(Ok _), state') -> (ok, state')
      _ -> loop state t

sortBody :: NonEmpty (Body.Spec a) -> NonEmpty (Body.Spec a)
sortBody = NonEmpty.sortBy comparer
  where
    comparer :: Body.Spec a -> Body.Spec a -> Ordering
    comparer Body.None Body.None = EQ
    comparer (Body.FMap _ Body.None) (Body.FMap _ Body.None) = EQ
    comparer (Body.FMap _ Body.None) _ = GT
    comparer _ (Body.FMap _ Body.None) = LT
    comparer Body.None _body = GT
    comparer _body Body.None = LT

evalSecurity :: NonEmpty (Security.Spec a) -> Secure.State -> (Result Security.Error a, Secure.State)
evalSecurity (h :| t) state = case Security.eval h state of
  (ok@(Ok _), s) -> (ok, s)
  _ -> loop state t
  where
    loop :: Secure.State -> [Security.Spec a] -> (Result Security.Error a, Secure.State)
    loop state [] = (Fail $ Security.SecureError Secure.ParseFail, state)
    loop state (h : t) = case Security.eval h state of
      (ok@(Ok _), state') -> (ok, state')
      _ -> loop state t

sortSecurity :: NonEmpty (Security.Spec a) -> NonEmpty (Security.Spec a)
sortSecurity = NonEmpty.sortBy comparer
  where
    comparer :: Security.Spec a -> Security.Spec a -> Ordering
    comparer Security.None Security.None = EQ
    comparer (Security.FMap _ Security.None) (Security.FMap _ Security.None) = EQ
    comparer Security.None _ = GT
    comparer (Security.FMap _ Security.None) _ = GT
    comparer _ Security.None = LT
    comparer _ (Security.FMap _ Security.None) = LT

toOpenAPI ::
  Server route ->
  OAPI.OpenApi
toOpenAPI Server {info, description, routes, url} =
  mempty
    { OAPI._openApiInfo = info,
      OAPI._openApiServers =
        [ OAPI.Server
            (Text.intercalate "/" url)
            description
            mempty
        ],
      OAPI._openApiPaths = pathItemsToOpenAPIPaths routes,
      OAPI._openApiOpenapi = OpenApiSpecVersion {getVersion = Version.Version [3, 0, 3] []}
    }

pathItemsToOpenAPIPaths :: Routes routes -> InsOrdHashMap.InsOrdHashMap FilePath OAPI.PathItem
pathItemsToOpenAPIPaths Nil = InsOrdHashMap.fromList []
pathItemsToOpenAPIPaths (h :& t) = let (filePath, pathItem) = toOpenAPIPathItem h in InsOrdHashMap.insert filePath pathItem $ pathItemsToOpenAPIPaths t

toOpenAPIPathItem :: Route route -> (FilePath, OAPI.PathItem)
toOpenAPIPathItem (Route @route summary description get post put delete) = (pathName, pathItem)
  where
    pathName :: FilePath
    pathName = renderPath $ Path.parser @route

    pathItem :: OAPI.PathItem
    pathItem =
      mempty
        { OAPI._pathItemSummary = summary,
          OAPI._pathItemDescription = description,
          OAPI._pathItemGet = fmap toGetOperation get,
          OAPI._pathItemPost = fmap toPostOperation post,
          OAPI._pathItemPut = fmap toPutOperation put,
          OAPI._pathItemDelete = fmap toDeleteOperation delete
        }

toGetOperation :: GET m route security query headers responder -> OAPI.Operation
toGetOperation (GET @_ @route @security @query @headers @responder summary description _ _) =
  mempty
    { OAPI._operationParameters = toParameters (Path.parser @route, Query.parser @query, Headers.parser @headers),
      OAPI._operationResponses = toResponses $ Response.parser @responder,
      OAPI._operationSecurity = toSecurityRequirements $ Security.parser @security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toPostOperation :: POST m route security query body headers responder -> OAPI.Operation
toPostOperation (POST @_ @route @security @query @body @headers @responder summary description _ _) =
  mempty
    { OAPI._operationParameters = toParameters (Path.parser @route, Query.parser @query, Headers.parser @headers),
      OAPI._operationRequestBody = toOpenAPIRequestBody $ Body.parser @body,
      OAPI._operationResponses = toResponses $ Response.parser @responder,
      OAPI._operationSecurity = toSecurityRequirements $ Security.parser @security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toPutOperation :: PUT m route security query body headers responder -> OAPI.Operation
toPutOperation (PUT @_ @route @security @query @body @headers @responder summary description _ _) =
  mempty
    { OAPI._operationParameters = toParameters (Path.parser @route, Query.parser @query, Headers.parser @headers),
      OAPI._operationRequestBody = toOpenAPIRequestBody $ Body.parser @body,
      OAPI._operationResponses = toResponses $ Response.parser @responder,
      OAPI._operationSecurity = toSecurityRequirements $ Security.parser @security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toDeleteOperation :: DELETE m route security query headers responder -> OAPI.Operation
toDeleteOperation (DELETE @_ @route @security @query @headers @responder summary description _ _) =
  mempty
    { OAPI._operationParameters = toParameters (Path.parser @route, Query.parser @query, Headers.parser @headers),
      OAPI._operationResponses = toResponses $ Response.parser @responder,
      OAPI._operationSecurity = toSecurityRequirements $ Security.parser @security,
      OAPI._operationSummary = summary,
      OAPI._operationDescription = description
    }

toParameters :: (Path.Spec route, Query.Spec q, Headers.Spec h) -> [OAPI.Referenced OAPI.Param]
toParameters (path, query, headers) = pathParameters path <> queryParameters query <> headersParameters headers
  where
    pathParameters :: Path.Spec route -> [OAPI.Referenced OAPI.Param]
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

    queryParameters :: Query.Spec q -> [OAPI.Referenced OAPI.Param]
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

    headersParameters :: Headers.Spec h -> [OAPI.Referenced OAPI.Param]
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

toSecurityRequirements :: NonEmpty (Security.Spec s) -> [OAPI.SecurityRequirement]
toSecurityRequirements security = []

toOpenAPIRequestBody :: NonEmpty (Body.Spec b) -> Maybe (OAPI.Referenced OAPI.RequestBody)
toOpenAPIRequestBody body = Nothing

toResponses :: Response.Spec r -> OAPI.Responses
toResponses responder = mempty

renderPath :: Path.Spec a -> FilePath
renderPath path = case path of
  Path.FMap f p -> renderPath p
  Path.Pure _ -> mempty
  Path.Apply pf px -> renderPath pf <> renderPath px
  Path.Static t -> "/" <> Text.unpack t
  Path.Param @p name -> "/{" <> Text.unpack name <> "}"
