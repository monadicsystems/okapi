{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Endpoint where

import Control.Natural (type (~>))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.OpenApi qualified as OAPI
import Data.OpenApi.Declare qualified as OAPI
import Data.Proxy
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Okapi.Request (Request)
import Okapi.Script
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Okapi.Script.ResponderHeaders (Response, toWaiResponse)

data Endpoint p q h b r = Endpoint
  { method :: HTTP.StdMethod,
    pathScript :: Path.Script p,
    queryScript :: Query.Script q,
    headersScript :: Headers.Script h,
    bodyScript :: Body.Script b,
    responderScript :: Responder.Script r
  }

genOAPIPathItem :: Endpoint p q h b r -> (FilePath, OAPI.PathItem)
genOAPIPathItem endpoint = (pathName, pathItem)
  where
    pathName :: FilePath
    pathName = renderPath endpoint.pathScript

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
    parameters = pathParams endpoint.pathScript <> queryParams endpoint.queryScript <> headersParams endpoint.headersScript

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

data Plan m p q h b r = Plan
  { transformer :: m ~> IO,
    endpoint :: Endpoint p q h b r,
    handler :: Monad m => p -> q -> b -> h -> r -> m Response
  }

data Executable = Run (IO WAI.Response) | Null

type Compiler = Request -> Executable

data Artifacts = Artifacts
  { compiler :: Compiler,
    openAPIPathItem :: (FilePath, OAPI.PathItem)
  }

genArtifacts ::
  forall m p q h b r.
  Monad m =>
  Plan m p q h b r ->
  Artifacts
genArtifacts plan = Artifacts {..}
  where
    openAPIPathItem = genOAPIPathItem plan.endpoint
    compiler (method, path, query, body, headers) =
      if method == plan.endpoint.method
        then
          let pathResult = fst $ Path.eval plan.endpoint.pathScript path
              queryResult = fst $ Query.eval plan.endpoint.queryScript query
              bodyResult = fst $ Body.eval plan.endpoint.bodyScript body
              headersResult = fst $ Headers.eval plan.endpoint.headersScript headers
              responderResult = fst $ Responder.eval plan.endpoint.responderScript ()
           in case (pathResult, queryResult, bodyResult, headersResult, responderResult) of
                (Ok p, Ok q, Ok b, Ok h, Ok r) -> Run do
                  response <- transformer plan $ handler plan p q b h r
                  return $ toWaiResponse response
                _ -> Null
        else Null

data Info = Info
  { author :: Text.Text,
    name :: Text.Text
  }

data Server = Server
  { info :: Maybe Info,
    artifacts :: [Artifacts],
    defaultResponse :: WAI.Response
  }

data Options = Options

genApplication ::
  Options ->
  Server ->
  (WAI.Application, OAPI.OpenApi)
genApplication _ server = (app, spec)
  where
    spec = genOpenAPISpec server.artifacts
    app waiRequest respond = do
      -- This is where routing happens. May be ineffecient
      body <- WAI.strictRequestBody waiRequest
      let Right method = HTTP.parseMethod $ WAI.requestMethod waiRequest
          -- \^ TODO: Fix above. Not complete
          path = WAI.pathInfo waiRequest
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

genOpenAPISpec ::
  [Artifacts] ->
  OAPI.OpenApi
genOpenAPISpec = undefined

genJSClient ::
  Server ->
  BS.ByteString
genJSClient = undefined
