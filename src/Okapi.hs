{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Okapi is a micro web framework.
module Okapi
  ( run,
    serve,
    serveTLS,
    serveWebsockets,
    serveWebsocketsTLS,
    app,
    websocketsApp,
  )
where

import qualified Control.Applicative as Applicative
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Control.Monad.Zip as Zip
import qualified Crypto.Hash as Crypto
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteArray as Memory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either.Extra as Either
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.Functor as Functor
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Wai as WAI
import qualified Network.Wai.EventSource as WAI
import qualified Network.Wai.Handler.Warp as WAI hiding (FileInfo (..))
import qualified Network.Wai.Handler.WarpTLS as WAI
import qualified Network.Wai.Handler.WebSockets as WAI
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.Wai.Internal as WAI
import qualified Network.Wai.Middleware.Gzip as WAI
import qualified Network.Wai.Parse as WAI
import qualified Network.Wai.Test as WAI
import qualified Network.WebSockets as WebSockets
import qualified Okapi.Error as Error
import qualified Okapi.Event as Event
import qualified Okapi.HTTP as HTTP
import qualified Okapi.Middleware.Session as Session
import qualified Okapi.Request as Request
import qualified Okapi.Request.Body as Body
import qualified Okapi.Request.Headers as Headers
import qualified Okapi.Request.Method as Method
import qualified Okapi.Request.Path as Path
import qualified Okapi.Request.Query as Query
import qualified Okapi.Request.Vault as Vault
import qualified Okapi.Response as Response
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

-- $middleware
--
-- Middlewares allow you to modify the behavior of Okapi handlers.
-- Middlewares are functions that take a handler and return another handler.
-- Middlewares can be composed with the fish operator @>=>@.
--
-- @
--  clearHeadersMiddleware >=> pathPrefix ["jello"] :: forall m. Middleware m
-- @

applyMiddlewares :: HTTP.MonadHTTP m => [m () -> m ()] -> (m () -> m ())
applyMiddlewares middlewares handler =
  List.foldl (\handler middleware -> middleware handler) handler middlewares

-- TODO: Is this needed? Idea taken from OCaml Dream framework

scope :: HTTP.MonadHTTP m => Path.Path -> [m () -> m ()] -> (m () -> m ())
scope prefix middlewares handler = Path.path `Error.is` prefix >> applyMiddlewares middlewares handler

clearHeadersMiddleware :: Response.MonadResponse m => m () -> m ()
clearHeadersMiddleware handler = do
  Response.setHeaders []
  handler

prefixPathMiddleware :: Request.MonadRequest m => Path.Path -> (m () -> m ())
prefixPathMiddleware prefix handler = Path.path `Error.is` prefix >> handler

-- $wai
--
-- These functions are for interfacing with WAI (Web Application Interface).

run :: Monad m => (forall a. m a -> IO a) -> HTTP.HTTPT m () -> IO ()
run = serve 3000 Response.notFound

serve ::
  Monad m =>
  -- | Port
  Int ->
  -- | Default Response
  Response.Response ->
  -- | Monad unlift function
  (forall a. m a -> IO a) ->
  -- | Parser
  HTTP.HTTPT m () ->
  IO ()
serve port defaultResponse hoister serverT = WAI.run port $ app defaultResponse hoister serverT

serveTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  Response.Response ->
  (forall a. m a -> IO a) ->
  HTTP.HTTPT m () ->
  IO ()
serveTLS tlsSettings settings defaultResponse hoister serverT = WAI.runTLS tlsSettings settings $ app defaultResponse hoister serverT

serveWebsockets ::
  Monad m =>
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  Int ->
  Response.Response ->
  (forall a. m a -> IO a) ->
  HTTP.HTTPT m () ->
  IO ()
serveWebsockets connSettings serverApp port defaultResponse hoister serverT = WAI.run port $ websocketsApp connSettings serverApp defaultResponse hoister serverT

serveWebsocketsTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  Response.Response ->
  (forall a. m a -> IO a) ->
  HTTP.HTTPT m () ->
  IO ()
serveWebsocketsTLS tlsSettings settings connSettings serverApp defaultResponse hoister serverT = WAI.runTLS tlsSettings settings $ websocketsApp connSettings serverApp defaultResponse hoister serverT

-- | Turns a parser into a WAI application
app ::
  Monad m =>
  -- | The default response to pure if parser fails
  Response.Response ->
  -- | Function for "unlifting" monad inside @HTTPT@ to @IO@ monad
  (forall a. m a -> IO a) ->
  -- | The parser used to equals the request
  HTTP.HTTPT m () ->
  WAI.Application
app defaultResponse hoister serverT waiRequest respond = do
  initialState <- waiRequestToState waiRequest
  (failureOrUnit, (request, response)) <- (StateT.runStateT . ExceptT.runExceptT . HTTP.runServerT $ Morph.hoist hoister serverT) initialState
  case failureOrUnit of
    Left error -> errorToWaiApp defaultResponse error waiRequest respond
    Right () -> responseToWaiApp response waiRequest respond
  where
    responseToWaiApp :: Response.Response -> WAI.Application
    responseToWaiApp Response.Response {..} waiRequest respond = case body of
      Response.Raw raw -> respond $ WAI.responseLBS (toEnum $ fromEnum status) headers raw
      Response.File filePath -> respond $ WAI.responseFile (toEnum $ fromEnum status) headers filePath Nothing
      Response.EventSource eventSource -> (WAI.gzip WAI.def $ Event.eventSourceAppUnagiChan eventSource) waiRequest respond

    errorToWaiApp :: Response.Response -> Error.Error -> WAI.Application
    errorToWaiApp _ Error.Error {..} waiRequest respond = case body of
      Error.Raw raw -> respond $ WAI.responseLBS (toEnum $ fromEnum status) headers raw
      Error.File filePath -> respond $ WAI.responseFile (toEnum $ fromEnum status) headers filePath Nothing
      Error.EventSource eventSource -> (WAI.gzip WAI.def $ Event.eventSourceAppUnagiChan eventSource) waiRequest respond
    errorToWaiApp defaultResponse Error.Next waiRequest respond = responseToWaiApp defaultResponse waiRequest respond

    waiRequestToState :: WAI.Request -> IO (Request.Request, Response.Response)
    waiRequestToState waiRequest = do
      body <- case lookup "Content-Type" $ WAI.requestHeaders waiRequest of
        Just "multipart/form-data" -> Body.Multipart <$> WAI.parseRequestBody WAI.lbsBackEnd waiRequest
        _ -> Body.Raw <$> WAI.strictRequestBody waiRequest -- TODO: Use lazy request body???
      let method = Just $ WAI.requestMethod waiRequest
          path = WAI.pathInfo waiRequest
          query = map (\case (name, Nothing) -> (name, Query.QueryFlag); (name, Just txt) -> (name, Query.QueryParam txt)) $ HTTP.queryToQueryText $ WAI.queryString waiRequest
          headers = WAI.requestHeaders waiRequest
          vault = WAI.vault waiRequest
          request = Request.Request {..}
      pure (request, Response.ok)

-- | Turns a parsers into a WAI application with WebSocket functionality
-- See __ for information on how to create a WebSocket server
websocketsApp ::
  Monad m =>
  -- | Connection options configuration for the WebSocket server
  WebSockets.ConnectionOptions ->
  -- | The server to use for handling WebSocket connections
  WebSockets.ServerApp ->
  Response.Response ->
  (forall a. m a -> IO a) ->
  HTTP.HTTPT m () ->
  WAI.Application
websocketsApp connSettings serverApp defaultResponse hoister serverT =
  let backupApp = app defaultResponse hoister serverT
   in WebSockets.websocketsOr connSettings serverApp backupApp

-- $routing
--
-- Okapi implements routes and type-safe relative URLs using bidirectional pattern synonyms and view patterns.
-- Routing can be extended to dispatch on any property of the request, including method, path, query, headers, and even body.
-- By default, Okapi provides a @route@ function for dispatching on the path of the request.

route :: Request.MonadRequest m => m a -> (a -> m ()) -> m ()
route parser dispatcher = parser >>= dispatcher

viewQuery :: Text.Text -> Query.Query -> (Maybe Query.QueryValue, Query.Query)
viewQuery name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

viewQueryParam :: Web.FromHttpApiData a => Text.Text -> Query.Query -> (Maybe a, Query.Query)
viewQueryParam name query = case List.lookup name query of
  Just (Query.QueryParam param) -> case Web.parseQueryParamMaybe param of
    Nothing -> (Nothing, query)
    Just value -> (Just value, List.delete (name, Query.QueryParam param) query)
  _ -> (Nothing, query)

-- $relativeURLs
--
-- Relative URLs are useful when we want to refer to other locations within our app.
-- Thanks to bidirectional patterns, we can use the same pattern to deconstruct an incoming request
-- AND construct the relative URL that leads to itself.

data RelURL = RelURL Path.Path Query.Query

-- TODO: Use ToURL typeclass for Path and Query, then combine for RelURL??
renderRelURL :: RelURL -> Text.Text
renderRelURL (RelURL path query) = case (path, query) of
  ([], []) -> ""
  ([], q) -> "?" <> renderQuery q
  (p, []) -> renderPath p
  (p, q) -> renderPath p <> "?" <> renderQuery q

renderPath :: Path.Path -> Text.Text
renderPath [] = "/"
renderPath (pathSeg : path) = "/" <> pathSeg <> loop path
  where
    loop :: Path.Path -> Text.Text
    loop [] = ""
    loop (pathSeg : path) = "/" <> pathSeg <> loop path

renderQuery :: Query.Query -> Text.Text
renderQuery [] = ""
renderQuery ((name, Query.QueryFlag) : query) = name <> "&" <> renderQuery query
renderQuery ((name, Query.QueryParam value) : query) = name <> "=" <> value <> "&" <> renderQuery query

parseRelURL :: Text.Text -> Maybe RelURL
parseRelURL possibleRelURL = Either.eitherToMaybe $
  flip Atto.parseOnly possibleRelURL $ do
    path <- Combinators.many pathSeg
    maybeQueryStart <- Combinators.optional $ Atto.char '?'
    case maybeQueryStart of
      Nothing -> pure $ RelURL path []
      Just _ -> do
        query <- Combinators.many queryParam
        pure $ RelURL path query
  where
    pathSeg :: Atto.Parser Text.Text
    pathSeg = do
      Atto.char '/'
      Atto.takeWhile (\c -> c /= '/' && c /= '?')

    queryParam :: Atto.Parser (Text.Text, Query.QueryValue)
    queryParam = do
      queryParamName <- Atto.takeWhile (\c -> c /= '=' && c /= '&')
      mbEquals <- Combinators.optional $ Atto.char '='
      case mbEquals of
        Nothing -> pure (queryParamName, Query.QueryFlag)
        Just _ -> do
          queryParamValue <- Atto.takeWhile (/= '&')
          pure (queryParamName, Query.QueryParam queryParamValue)
