{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Lazy as StateT
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
import qualified Okapi.Internal.Event as Event
import qualified Okapi.Internal.HTTP as HTTP
import qualified Okapi.Internal.Request.Body as Body
import qualified Okapi.Internal.Request.Headers as Headers
import qualified Okapi.Internal.Request.Method as Method
import qualified Okapi.Internal.Request.Path as Path
import qualified Okapi.Internal.Request.Query as Query
import qualified Okapi.Internal.Request.Vault as Vault
import qualified Okapi.Internal.Response as Response
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

applyMiddlewares :: HTTP.Parser m => [m () -> m ()] -> (m () -> m ())
applyMiddlewares middlewares handler =
  List.foldl (\handler middleware -> middleware handler) handler middlewares

-- TODO: Is this needed? Idea taken from OCaml Dream framework

scope :: HTTP.Parser m => Request.Path -> [m () -> m ()] -> (m () -> m ())
scope prefix middlewares handler = Path.use `Error.is` prefix >> applyMiddlewares middlewares handler

clearHeadersMiddleware :: Response.Parser m => m () -> m ()
clearHeadersMiddleware handler = do
  Response.setHeaders []
  handler

prefixPathMiddleware :: Request.Parser m => Request.Path -> (m () -> m ())
prefixPathMiddleware prefix handler = Path.use `Error.is` prefix >> handler

-- $wai
--
-- These functions are for interfacing with WAI (Web Application Interface).

run :: Monad m => (forall a. m a -> IO a) -> HTTPT m () -> IO ()
run = serve 3000 Response.notFound

serve ::
  Monad m =>
  -- | Port
  Int ->
  -- | Default Response
  HTTP.Response ->
  -- | Monad unlift function
  (forall a. m a -> IO a) ->
  -- | Parser
  HTTPT m () ->
  IO ()
serve port defaultResponse hoister serverT = WAI.run port $ app defaultResponse hoister serverT

serveTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  HTTP.Response ->
  (forall a. m a -> IO a) ->
  HTTPT m () ->
  IO ()
serveTLS tlsSettings settings defaultResponse hoister serverT = WAI.runTLS tlsSettings settings $ app defaultResponse hoister serverT

serveWebsockets ::
  Monad m =>
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  Int ->
  HTTP.Response ->
  (forall a. m a -> IO a) ->
  HTTPT m () ->
  IO ()
serveWebsockets connSettings serverApp port defaultResponse hoister serverT = WAI.run port $ websocketsApp connSettings serverApp defaultResponse hoister serverT

serveWebsocketsTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  HTTP.Response ->
  (forall a. m a -> IO a) ->
  HTTPT m () ->
  IO ()
serveWebsocketsTLS tlsSettings settings connSettings serverApp defaultResponse hoister serverT = WAI.runTLS tlsSettings settings $ websocketsApp connSettings serverApp defaultResponse hoister serverT

-- | Turns a parser into a WAI application
app ::
  Monad m =>
  -- | The default response to pure if parser fails
  HTTP.Response ->
  -- | Function for "unlifting" monad inside @HTTPT@ to @IO@ monad
  (forall a. m a -> IO a) ->
  -- | The parser used to equals the request
  HTTPT m () ->
  WAI.Application
app defaultResponse hoister serverT waiRequest respond = do
  initialState <- waiRequestToState waiRequest
  (failureOrUnit, (request, response)) <- (StateT.runStateT . ExceptT.runExceptT . runServerT $ Morph.hoist hoister serverT) initialState
  case failureOrUnit of
    Left error -> errorToWaiApp defaultResponse error waiRequest respond
    Right () -> responseToWaiApp response waiRequest respond
  where
    responseToWaiApp :: HTTP.Response -> WAI.Application
    responseToWaiApp HTTP.Response {..} waiRequest respond = case body of
      Response.Raw raw -> respond $ WAI.responseLBS (toEnum $ fromEnum status) headers raw
      Response.File filePath -> respond $ WAI.responseFile (toEnum $ fromEnum status) headers filePath Nothing
      Response.EventSource eventSource -> (WAI.gzip WAI.def $ Event.eventSourceAppUnagiChan eventSource) waiRequest respond

    errorToWaiApp :: HTTP.Response -> HTTP.Error -> WAI.Application
    errorToWaiApp _ HTTP.Error {..} waiRequest respond = case body of
      Error.Raw raw -> respond $ WAI.responseLBS (toEnum $ fromEnum status) headers raw
      Error.File filePath -> respond $ WAI.responseFile (toEnum $ fromEnum status) headers filePath Nothing
      Error.EventSource eventSource -> (WAI.gzip WAI.def $ Event.eventSourceAppUnagiChan eventSource) waiRequest respond
    errorToWaiApp defaultResponse HTTP.Next waiRequest respond = responseToWaiApp defaultResponse waiRequest respond

    waiRequestToState :: WAI.Request -> IO (HTTP.Request, HTTP.Response)
    waiRequestToState waiRequest = do
      body <- case lookup "Content-Type" $ WAI.requestHeaders waiRequest of
        Just "multipart/form-data" -> Request.Multipart <$> WAI.parseRequestBody WAI.lbsBackEnd waiRequest
        _ -> Request.Raw <$> WAI.strictRequestBody waiRequest -- TODO: Use lazy request body???
      let method = Just $ WAI.requestMethod waiRequest
          path = WAI.pathInfo waiRequest
          query = map (\case (name, Nothing) -> (name, Query.Flag); (name, Just txt) -> (name, Query.Param txt)) $ HTTP.queryToQueryText $ WAI.queryString waiRequest
          headers = WAI.requestHeaders waiRequest
          vault = WAI.vault waiRequest
          request = HTTP.Request {..}
      pure (request, Response.ok)

-- | Turns a parsers into a WAI application with WebSocket functionality
-- See __ for information on how to create a WebSocket server
websocketsApp ::
  Monad m =>
  -- | Connection options configuration for the WebSocket server
  WebSockets.ConnectionOptions ->
  -- | The server to use for handling WebSocket connections
  WebSockets.ServerApp ->
  HTTP.Response ->
  (forall a. m a -> IO a) ->
  HTTPT m () ->
  WAI.Application
websocketsApp connSettings serverApp defaultResponse hoister serverT =
  let backupApp = app defaultResponse hoister serverT
   in WebSockets.websocketsOr connSettings serverApp backupApp

-- $routing
--
-- Okapi implements routes and type-safe relative URLs using bidirectional pattern synonyms and view patterns.
-- Routing can be extended to dispatch on any property of the request, including method, path, query, headers, and even body.
-- By default, Okapi provides a @route@ function for dispatching on the path of the request.

route :: Request.Parser m => m a -> (a -> m ()) -> m ()
route parser dispatcher = parser >>= dispatcher

viewQuery :: Text.Text -> Request.Query -> (Maybe Query.Value, Request.Query)
viewQuery name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

viewQueryParam :: Web.FromHttpApiData a => Text.Text -> Request.Query -> (Maybe a, Request.Query)
viewQueryParam name query = case List.lookup name query of
  Just (Query.Param param) -> case Web.parseQueryParamMaybe param of
    Nothing -> (Nothing, query)
    Just value -> (Just value, List.delete (name, Query.Param param) query)
  _ -> (Nothing, query)

-- $relativeURLs
--
-- Relative URLs are useful when we want to refer to other locations within our app.
-- Thanks to bidirectional patterns, we can use the same pattern to deconstruct an incoming request
-- AND construct the relative URL that leads to itself.

data RelURL = RelURL Request.Path Request.Query

-- TODO: Use ToURL typeclass for Path and Query, then combine for RelURL??
renderRelURL :: RelURL -> Text.Text
renderRelURL (RelURL path query) = case (path, query) of
  ([], []) -> ""
  ([], q) -> "?" <> renderQuery q
  (p, []) -> renderPath p
  (p, q) -> renderPath p <> "?" <> renderQuery q

renderPath :: Request.Path -> Text.Text
renderPath [] = "/"
renderPath (pathSeg : path) = "/" <> pathSeg <> loop path
  where
    loop :: Request.Path -> Text.Text
    loop [] = ""
    loop (pathSeg : path) = "/" <> pathSeg <> loop path

renderQuery :: Request.Query -> Text.Text
renderQuery [] = ""
renderQuery ((name, Query.Flag) : query) = name <> "&" <> renderQuery query
renderQuery ((name, Query.Param value) : query) = name <> "=" <> value <> "&" <> renderQuery query

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

    queryParam :: Atto.Parser (Text.Text, Query.Value)
    queryParam = do
      queryParamName <- Atto.takeWhile (\c -> c /= '=' && c /= '&')
      mbEquals <- Combinators.optional $ Atto.char '='
      case mbEquals of
        Nothing -> pure (queryParamName, Query.Flag)
        Just _ -> do
          queryParamValue <- Atto.takeWhile (/= '&')
          pure (queryParamName, Query.Param queryParamValue)

-- | A concrete implementation of the @MonadHTTP@ type constraint.
newtype HTTPT m a = HTTPT {runServerT :: Except.ExceptT HTTP.Error (State.StateT (HTTP.Request, HTTP.Response) m) a}
  deriving newtype
    ( Except.MonadError HTTP.Error
    )

instance Functor m => Functor (HTTPT m) where
  fmap :: (a -> b) -> HTTPT m a -> HTTPT m b
  fmap f okapiT =
    HTTPT . Except.ExceptT . State.StateT $
      fmap (\ ~(a, s') -> (f <$> a, s'))
        . State.runStateT (Except.runExceptT $ runServerT okapiT)
  {-# INLINE fmap #-}

instance Monad m => Applicative (HTTPT m) where
  pure :: Monad m => a -> HTTPT m a
  pure x = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
  (<*>) :: Monad m => HTTPT m (a -> b) -> HTTPT m a -> HTTPT m b
  (HTTPT (Except.ExceptT (State.StateT mf))) <*> (HTTPT (Except.ExceptT (State.StateT mx))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherF, s') <- mf s
    case eitherF of
      Left error -> pure (Left error, s)
      Right f -> do
        ~(eitherX, s'') <- mx s'
        case eitherX of
          Left error' -> pure (Left error', s')
          Right x -> pure (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}
  (*>) :: Monad m => HTTPT m a -> HTTPT m b -> HTTPT m b
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance Monad m => Applicative.Alternative (HTTPT m) where
  empty :: Monad m => HTTPT m a
  empty = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Left HTTP.Next, s)
  {-# INLINE empty #-}
  (<|>) :: Monad m => HTTPT m a -> HTTPT m a -> HTTPT m a
  (HTTPT (Except.ExceptT (State.StateT mx))) <|> (HTTPT (Except.ExceptT (State.StateT my))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left HTTP.Next -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left HTTP.Next -> pure (Left HTTP.Next, s)
          Left error@HTTP.Error {} -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@HTTP.Error {} -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE (<|>) #-}

instance Monad m => Monad (HTTPT m) where
  return :: Monad m => a -> HTTPT m a
  return = pure
  {-# INLINEABLE return #-}
  (>>=) :: Monad m => HTTPT m a -> (a -> HTTPT m b) -> HTTPT m b
  (HTTPT (Except.ExceptT (State.StateT mx))) >>= f = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherX, s') <- mx s
    case eitherX of
      Left error -> pure (Left error, s)
      Right x -> do
        ~(eitherResult, s'') <- State.runStateT (Except.runExceptT $ runServerT $ f x) s'
        case eitherResult of
          Left error' -> pure (Left error', s')
          Right res -> pure (Right res, s'')
  {-# INLINEABLE (>>=) #-}

instance Monad m => Monad.MonadPlus (HTTPT m) where
  mzero :: Monad m => HTTPT m a
  mzero = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Left HTTP.Next, s)
  {-# INLINE mzero #-}
  mplus :: Monad m => HTTPT m a -> HTTPT m a -> HTTPT m a
  (HTTPT (Except.ExceptT (State.StateT mx))) `mplus` (HTTPT (Except.ExceptT (State.StateT my))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left HTTP.Next -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left HTTP.Next -> pure (Left HTTP.Next, s)
          Left error@HTTP.Error {} -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@HTTP.Error {} -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance Reader.MonadReader r m => Reader.MonadReader r (HTTPT m) where
  ask :: Reader.MonadReader r m => HTTPT m r
  ask = Morph.lift Reader.ask
  local :: Reader.MonadReader r m => (r -> r) -> HTTPT m a -> HTTPT m a
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either HTTP.Error a, (HTTP.Request, HTTP.Response)) -> n (Either HTTP.Error b, (HTTP.Request, HTTP.Response))) -> HTTPT m a -> HTTPT n b
      mapOkapiT f okapiT = HTTPT . Except.ExceptT . State.StateT $ f . State.runStateT (Except.runExceptT $ runServerT okapiT)
  reader :: Reader.MonadReader r m => (r -> a) -> HTTPT m a
  reader = Morph.lift . Reader.reader

instance Logger.MonadLogger m => Logger.MonadLogger (HTTPT m) where
  monadLoggerLog :: Logger.ToLogStr msg => Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> HTTPT m ()
  monadLoggerLog loc logSrc logLvl msg = Morph.lift $ Logger.monadLoggerLog loc logSrc logLvl msg

instance IO.MonadIO m => IO.MonadIO (HTTPT m) where
  liftIO :: IO.MonadIO m => IO a -> HTTPT m a
  liftIO = Morph.lift . IO.liftIO

instance Morph.MonadTrans HTTPT where
  lift :: Monad m => m a -> HTTPT m a
  lift action = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor HTTPT where
  hoist :: Monad m => (forall a. m a -> n a) -> HTTPT m b -> HTTPT n b
  hoist nat okapiT = HTTPT . Except.ExceptT . State.StateT $ nat . State.runStateT (Except.runExceptT $ runServerT okapiT)

instance Monad m => Method.State (HTTPT m) where
  get :: Monad m => HTTPT m Request.Method
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ HTTP.method req, s)
  put :: Monad m => Request.Method -> HTTPT m ()
  put method' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {HTTP.method = method'}, res))

instance Monad m => Path.State (HTTPT m) where
  get :: Monad m => HTTPT m Request.Path
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ HTTP.path req, s)
  put :: Monad m => Request.Path -> HTTPT m ()
  put path' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {HTTP.path = path'}, res))

instance Monad m => Headers.State (HTTPT m) where
  get :: Monad m => HTTPT m Request.Headers
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ HTTP.headers (req :: HTTP.Request), s)
  put :: Monad m => Request.Headers -> HTTPT m ()
  put headers' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {HTTP.headers = headers'} :: HTTP.Request, res))

instance Monad m => Query.State (HTTPT m) where
  get :: Monad m => HTTPT m Request.Query
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ HTTP.query req, s)
  put :: Monad m => Request.Query -> HTTPT m ()
  put query' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {HTTP.query = query'}, res))

instance Monad m => Body.State (HTTPT m) where
  get :: Monad m => HTTPT m Request.Body
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ HTTP.body (req :: HTTP.Request), s)
  put :: Monad m => Request.Body -> HTTPT m ()
  put body' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {HTTP.body = body'} :: HTTP.Request, res))

instance Monad m => Vault.State (HTTPT m) where
  get :: Monad m => HTTPT m Vault.Vault
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ HTTP.vault req, s)
  put :: Monad m => Vault.Vault -> HTTPT m ()
  put vault = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {HTTP.vault = vault}, res))

instance Monad m => Response.State (HTTPT m) where
  get :: Monad m => HTTPT m HTTP.Response
  get =
    HTTPT
      . Except.ExceptT
      . State.StateT
      $ \state@(_, res) -> pure (Right res, state)
  put :: Monad m => HTTP.Response -> HTTPT m ()
  put newRes =
    HTTPT
      . Except.ExceptT
      . State.StateT
      $ \(req, _) -> pure (Right (), (req, newRes))
