{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi
  ( run,
    runTLS,
    runWebsockets,
    runWebsocketsTLS,
    -- SERVER SENT EVENTS
    newEventSource,
    sendValue,
    sendEvent,
    -- TYPES
    MonadOkapi,
    OkapiT,
    State,
    Request,
    Response (..),
    ResponseBody (..),
    Failure,
    Path,
    Headers,
    QueryItem,
    Query,
    Cookie,
    Cookies,
    ToSSE (..),
    Event (..),
    Chan,
    EventSource,
    URL (..),
  )
where

import qualified Control.Applicative as Applicative
import qualified Control.Applicative.Combinators as Combinators
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
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
import qualified Data.Text.Encoding.Base64 as Base64
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wai.EventSource as WAI
import qualified Network.Wai.Handler.Warp as WAI
import qualified Network.Wai.Handler.WarpTLS as WAI
import qualified Network.Wai.Handler.WebSockets as WAI
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.Wai.Internal as WAI
import qualified Network.Wai.Middleware.Gzip as WAI
import qualified Network.WebSockets as WebSockets
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

-- TYPES.HS

type MonadOkapi m =
  ( Functor m,
    Applicative m,
    Applicative.Alternative m,
    Monad m,
    Monad.MonadPlus m,
    Except.MonadError Failure m,
    State.MonadState State m
  )

newtype OkapiT m a = OkapiT {unOkapiT :: Except.ExceptT Failure (State.StateT State m) a}
  deriving newtype
    ( Except.MonadError Failure,
      State.MonadState State
    )

instance Functor m => Functor (OkapiT m) where
  fmap :: (a -> b) -> OkapiT m a -> OkapiT m b
  fmap f okapiT =
    OkapiT . Except.ExceptT . State.StateT $
      ( fmap (\ ~(a, s') -> (f <$> a, s'))
          . State.runStateT (Except.runExceptT $ unOkapiT okapiT)
      )
  {-# INLINE fmap #-}

instance Monad m => Applicative (OkapiT m) where
  pure x = OkapiT . Except.ExceptT . State.StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
  (OkapiT (Except.ExceptT (State.StateT mf))) <*> (OkapiT (Except.ExceptT (State.StateT mx))) = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherF, s') <- mf s
    case eitherF of
      Left error -> pure (Left error, s)
      Right f -> do
        ~(eitherX, s'') <- mx s'
        case eitherX of
          Left error' -> pure (Left error', s')
          Right x -> pure (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance Monad m => Applicative.Alternative (OkapiT m) where
  empty = OkapiT . Except.ExceptT . State.StateT $ \s -> pure (Left Skip, s)
  {-# INLINE empty #-}
  (OkapiT (Except.ExceptT (State.StateT mx))) <|> (OkapiT (Except.ExceptT (State.StateT my))) = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Skip -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Skip -> pure (Left Skip, s)
          Left error@(Error _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Error _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE (<|>) #-}

instance Monad m => Monad (OkapiT m) where
  return = pure
  {-# INLINEABLE return #-}
  (OkapiT (Except.ExceptT (State.StateT mx))) >>= f = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherX, s') <- mx s
    case eitherX of
      Left error -> pure (Left error, s)
      Right x -> do
        ~(eitherResult, s'') <- State.runStateT (Except.runExceptT $ unOkapiT $ f x) s'
        case eitherResult of
          Left error' -> pure (Left error', s')
          Right res -> pure (Right res, s'')
  {-# INLINEABLE (>>=) #-}

instance Monad m => Monad.MonadPlus (OkapiT m) where
  mzero = OkapiT . Except.ExceptT . State.StateT $ \s -> pure (Left Skip, s)
  {-# INLINE mzero #-}
  (OkapiT (Except.ExceptT (State.StateT mx))) `mplus` (OkapiT (Except.ExceptT (State.StateT my))) = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Skip -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Skip -> pure (Left Skip, s)
          Left error@(Error _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Error _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance Reader.MonadReader r m => Reader.MonadReader r (OkapiT m) where
  ask = Morph.lift Reader.ask
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either Failure a, State) -> n (Either Failure b, State)) -> OkapiT m a -> OkapiT n b
      mapOkapiT f okapiT = OkapiT . Except.ExceptT . State.StateT $ f . State.runStateT (Except.runExceptT $ unOkapiT okapiT)
  reader = Morph.lift . Reader.reader

instance Morph.MonadTrans OkapiT where
  lift :: Monad m => m a -> OkapiT m a
  lift action = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor OkapiT where
  hoist :: Monad m => (forall a. m a -> n a) -> OkapiT m b -> OkapiT n b
  hoist nat okapiT = OkapiT . Except.ExceptT . State.StateT $ (nat . State.runStateT (Except.runExceptT $ unOkapiT okapiT))

-- TODO: ADD Text field to Skip for logging
data Failure = Skip | Error Response

instance Show Failure where
  show Skip = "Skipped"
  show (Error _) = "Error returned"

data State = State
  { stateRequest :: Request,
    -- TODO: Remove state checkers??
    stateRequestMethodParsed :: Bool, -- Use Maybe instead of State Checks
    stateRequestBodyParsed :: Bool,
    stateResponded :: Bool,
    stateVault :: Vault.Vault
    -- add HTTPDataStore???
  }

data Request = Request
  { requestMethod :: Method,
    requestPath :: Path,
    requestQuery :: Query,
    requestBody :: Body,
    requestHeaders :: Headers
  }

data Response = Response
  { responseStatus :: Status,
    responseHeaders :: Headers,
    responseBody :: ResponseBody
  }

data ResponseBody
  = ResponseBodyRaw LBS.ByteString
  | ResponseBodyFile FilePath
  | ResponseBodyEventSource EventSource

type Method = HTTP.Method

type Path = [Text.Text]

type Query = [QueryItem]

type QueryItem = (Text.Text, QueryValue)

data QueryValue = QueryParam Text.Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

type Body = LBS.ByteString

type Headers = [Header]

type Header = (HeaderName, BS.ByteString)

type HeaderName = HTTP.HeaderName

type Cookie = (Text.Text, Text.Text)

type Cookies = [Cookie]

type Status = Natural.Natural

data Event
  = Event
      { eventName :: Maybe Text.Text,
        eventID :: Maybe Text.Text,
        eventData :: LBS.ByteString
      }
  | CommentEvent LBS.ByteString
  | CloseEvent
  deriving (Show, Eq)

class ToSSE a where
  toSSE :: a -> Event

type Chan a = (Unagi.InChan a, Unagi.OutChan a)

type EventSource = Chan Event

newtype URL = URL {unURL :: Text.Text}
  deriving newtype (String.IsString, Semigroup, Monoid, Eq, Ord, Show)

run :: Monad m => (forall a. m a -> IO a) -> Response -> Int -> OkapiT m Response -> IO ()
run hoister defaultResponse port okapiT = do
  print $ "Running Okapi App on port " <> show port
  WAI.run port $ app hoister defaultResponse okapiT

runTLS :: Monad m => (forall a. m a -> IO a) -> Response -> WAI.TLSSettings -> WAI.Settings -> OkapiT m Response -> IO ()
runTLS hoister defaultResponse tlsSettings settings okapiT = do
  print "Running servo on port 43"
  WAI.runTLS tlsSettings settings $ app hoister defaultResponse okapiT

runWebsockets :: Monad m => (forall a. m a -> IO a) -> Response -> Int -> OkapiT m Response -> WebSockets.ConnectionOptions -> WebSockets.ServerApp -> IO ()
runWebsockets hoister defaultResponse port okapiT connSettings serverApp = do
  print $ "Running Okapi App on port " <> show port
  WAI.run port $ websocketsApp hoister defaultResponse okapiT connSettings serverApp

runWebsocketsTLS :: Monad m => (forall a. m a -> IO a) -> Response -> WAI.TLSSettings -> WAI.Settings -> OkapiT m Response -> WebSockets.ConnectionOptions -> WebSockets.ServerApp -> IO ()
runWebsocketsTLS hoister defaultResponse tlsSettings settings okapiT connSettings serverApp = do
  WAI.runTLS tlsSettings settings $ websocketsApp hoister defaultResponse okapiT connSettings serverApp

-- APPLICATION.HS

-- | Turns a parser into a WAI application
app ::
  Monad m =>
  -- | Function for "unlifting" monad inside @OkapiT@ to @IO@ monad
  (forall a. m a -> IO a) ->
  -- | The default response to return if parser fails
  Response ->
  -- | The parser used to match the request
  OkapiT m Response ->
  WAI.Application
app hoister defaultResponse okapiT waiRequest respond = do
  state <- waiRequestToState waiRequest
  (eitherFailureOrResponse, _state) <- (State.runStateT . Except.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) state
  let response =
        case eitherFailureOrResponse of
          Left Skip -> defaultResponse
          Left (Error errorResponse) -> errorResponse
          Right succesfulResponse -> succesfulResponse
  responseToWaiApp response waiRequest respond
  where
    responseToWaiApp :: Response -> WAI.Application
    responseToWaiApp (Response {..}) waiRequest respond = case responseBody of
      ResponseBodyRaw body -> respond $ WAI.responseLBS (toEnum $ fromEnum responseStatus) responseHeaders body
      ResponseBodyFile filePath -> respond $ WAI.responseFile (toEnum $ fromEnum responseStatus) responseHeaders filePath Nothing
      ResponseBodyEventSource eventSource -> (WAI.gzip WAI.def $ eventSourceAppUnagiChan eventSource) waiRequest respond

    waiRequestToState :: WAI.Request -> IO State
    waiRequestToState waiRequest = do
      requestBody <- WAI.strictRequestBody waiRequest
      let requestMethod = WAI.requestMethod waiRequest
          requestPath = WAI.pathInfo waiRequest
          requestQuery = map (\case (name, Nothing) -> (name, QueryFlag); (name, Just txt) -> (name, QueryParam txt)) $ HTTP.queryToQueryText $ WAI.queryString waiRequest
          requestHeaders = WAI.requestHeaders waiRequest
          stateRequest = Request {..}
          stateRequestMethodParsed = False
          stateRequestBodyParsed = False
          stateResponded = False
          stateVault = WAI.vault waiRequest

      pure State {..}

-- | Turns a parsers into a WAI application with WebSocket functionality
-- See __ for information on how to create a WebSocket server
websocketsApp ::
  Monad m =>
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  -- | Connection options configuration for the WebSocket server
  WebSockets.ConnectionOptions ->
  -- | The server to use for handling WebSocket connections
  WebSockets.ServerApp ->
  WAI.Application
websocketsApp hoister defaultResponse okapiT connSettings serverApp =
  let backupApp = app hoister defaultResponse okapiT
   in WebSockets.websocketsOr connSettings serverApp backupApp

-- EVENT.HS

newEventSource :: IO EventSource
newEventSource = Unagi.newChan

sendValue :: ToSSE a => EventSource -> a -> IO ()
sendValue (inChan, _outChan) = Unagi.writeChan inChan . toSSE

sendEvent :: EventSource -> Event -> IO ()
sendEvent (inChan, _outChan) = Unagi.writeChan inChan

eventSourceAppUnagiChan :: EventSource -> WAI.Application
eventSourceAppUnagiChan (inChan, _outChan) req sendResponse = do
  outChan <- IO.liftIO $ Unagi.dupChan inChan
  eventSourceAppIO (eventToServerEvent <$> Unagi.readChan outChan) req sendResponse

-- BELOW IS INTERNAL

eventSourceAppIO :: IO WAI.ServerEvent -> WAI.Application
eventSourceAppIO src _ sendResponse =
  sendResponse $
    WAI.responseStream
      HTTP.status200
      [(HTTP.hContentType, "text/event-stream")]
      $ \sendChunk flush -> do
        flush
        Function.fix $ \loop -> do
          se <- src
          case eventToBuilder se of
            Nothing -> return ()
            Just b -> sendChunk b >> flush >> loop

eventToBuilder :: WAI.ServerEvent -> Maybe Builder.Builder
eventToBuilder (WAI.CommentEvent txt) = Just $ field commentField txt
eventToBuilder (WAI.RetryEvent n) = Just $ field retryField (Builder.string8 . show $ n)
eventToBuilder WAI.CloseEvent = Nothing
eventToBuilder (WAI.ServerEvent n i d) =
  Just $
    mappend (name n (evid i $ evdata (mconcat d) nl)) nl
  where
    name Nothing = id
    name (Just n') = mappend (field nameField n')
    evid Nothing = id
    evid (Just i') = mappend (field idField i')
    evdata d' = mappend (field dataField d')

nl :: Builder.Builder
nl = Builder.char7 '\n'

nameField, idField, dataField, retryField, commentField :: Builder.Builder
nameField = Builder.string7 "event:"
idField = Builder.string7 "id:"
dataField = Builder.string7 "data:"
retryField = Builder.string7 "retry:"
commentField = Builder.char7 ':'

-- | Wraps the text as a labeled field of an event stream.
field :: Builder.Builder -> Builder.Builder -> Builder.Builder
field l b = l `mappend` b `mappend` nl

eventToServerEvent :: Event -> WAI.ServerEvent
eventToServerEvent Event {..} =
  WAI.ServerEvent
    (Builder.byteString . Text.encodeUtf8 <$> eventName)
    (Builder.byteString . Text.encodeUtf8 <$> eventID)
    (Builder.word8 <$> LBS.unpack eventData)
eventToServerEvent (CommentEvent comment) = WAI.CommentEvent $ Builder.lazyByteString comment
eventToServerEvent CloseEvent = WAI.CloseEvent

-- MIDDLEWARE.HS

applyMiddlewares :: MonadOkapi m => [m Response -> m Response] -> m Response -> m Response
applyMiddlewares ms handler =
  Prelude.foldl (\handler m -> m handler) handler ms

addHeadersMiddleware :: MonadOkapi m => m Response -> m Response
addHeadersMiddleware handler = setHeaders [] <$> handler

prefixPathMiddeware :: MonadOkapi m => [Text.Text] -> (m Response -> m Response)
prefixPathMiddeware prefix handler = path prefix >> handler

-- | TODO: Is this needed? Idea taken from OCaml Dream framework
scope :: MonadOkapi m => [Text.Text] -> [m Response -> m Response] -> (m Response -> m Response)
scope prefix middlewares handler = do
  path prefix
  applyMiddlewares middlewares handler

-- PARSER.HS

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Okapi.Response
-- >>> import Okapi.Test

-- |
-- >>> let parser = get >> respond ok
-- >>> result <- testParserIO parser $ request GET "" "" []
-- >>> assertResponse is200 result
-- True
get :: forall m. MonadOkapi m => m ()
get = method HTTP.methodGet

-- |
-- >>> let parser = post >> respond ok
-- >>> result <- testParserIO parser (TestRequest "POST" [] "" "")
-- >>> assertResponse is200 result
-- True
post :: forall m. MonadOkapi m => m ()
post = method HTTP.methodPost

-- |
-- >>> let parser = Okapi.Parser.head >> respond ok
-- >>> result <- testParserIO parser (TestRequest "HEAD" [] "" "")
-- >>> assertResponse is200 result
-- True
head :: forall m. MonadOkapi m => m ()
head = method HTTP.methodHead

-- |
-- >>> let parser = put >> respond ok
-- >>> result <- testParserIO parser (TestRequest "PUT" [] "" "")
-- >>> assertResponse is200 result
-- True
put :: forall m. MonadOkapi m => m ()
put = method HTTP.methodPut

-- |
-- >>> let parser = delete >> respond ok
-- >>> result <- testParserIO parser (TestRequest "DELETE" [] "" "")
-- >>> assertResponse is200 result
-- True
delete :: forall m. MonadOkapi m => m ()
delete = method HTTP.methodDelete

-- |
-- >>> let parser = trace >> respond ok
-- >>> result <- testParserIO parser (TestRequest "TRACE" [] "" "")
-- >>> assertResponse is200 result
-- True
trace :: forall m. MonadOkapi m => m ()
trace = method HTTP.methodTrace

-- |
-- >>> let parser = connect >> respond ok
-- >>> result <- testParserIO parser (TestRequest "CONNECT" [] "" "")
-- >>> assertResponse is200 result
-- True
connect :: forall m. MonadOkapi m => m ()
connect = method HTTP.methodConnect

-- |
-- >>> let parser = options >> respond ok
-- >>> result <- testParserIO parser (TestRequest "OPTIONS" [] "" "")
-- >>> assertResponse is200 result
-- True
options :: forall m. MonadOkapi m => m ()
options = method HTTP.methodOptions

-- |
-- >>> let parser = patch >> respond ok
-- >>> result <- testParserIO parser (TestRequest "PATCH" [] "" "")
-- >>> assertResponse is200 result
-- True
patch :: forall m. MonadOkapi m => m ()
patch = method HTTP.methodPatch

-- |
-- >>> let parser = anyMethod >> respond ok
-- >>> result <- testParserIO parser (TestRequest "FOOBLAH" [] "" "")
-- >>> assertResponse is200 result
-- True
anyMethod :: forall m. MonadOkapi m => m ()
anyMethod = parseMethod >> pure ()

-- |
-- >>> let parser = method "CUSTOM" >> respond ok
-- >>> result <- testParserIO parser (TestRequest "CUSTOM" [] "" "")
-- >>> assertResponse is200 result
-- True
method :: forall m. MonadOkapi m => HTTP.Method -> m ()
method method = do
  method' <- parseMethod
  if method == method'
    then pure ()
    else skip

-- | Parses and discards a single path segment matching the given @Text@ value
--
-- >>> :{
-- parser = do
--   get
--   pathSeg "store"
--   pathSeg "clothing"
--   respond ok;
-- :}
--
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/store/clothing" "")
-- >>> assertResponse is200 result
-- True
pathSeg :: forall m. MonadOkapi m => Text.Text -> m ()
pathSeg goal = pathSegWith (goal ==)

-- | Parses and discards mutiple path segments matching the values and order of the given @[Text]@ value
--
-- >>> :{
-- parser = do
--   get
--   path ["store", "clothing"]
--   respond ok
-- :}
--
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/store/clothing" "")
-- >>> assertResponse is200 result
-- True
path :: forall m. MonadOkapi m => [Text.Text] -> m ()
path = mapM_ pathSeg

-- | Parses a single path segment and returns it as a Haskell value of the specified type
--
-- >>> :set -XTypeApplications
-- >>> :{
-- parser = do
--   get
--   pathSeg "product"
--   productID <- pathParam @Int
--   respond $ json productID $ ok;
-- :}
--
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/product/242301" "")
-- >>> assertResponse is200 result
-- True
pathParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => m a
pathParam = do
  pathSeg <- parsePathSeg
  maybe skip pure (Web.parseUrlPieceMaybe pathSeg)

-- | Parses a single path segment as raw @Text@.
-- Use this instead of @pathParam@ if you want to process the path segment yourself
--
-- >>> :{
-- parser = do
--   get
--   pathSeg "product"
--   productID <- pathParamRaw
--   respond $ json productID $ ok
-- :}
--
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/product/242301" "")
-- >>> assertResponse is200 result
-- True
pathParamRaw :: forall m. MonadOkapi m => m Text.Text
pathParamRaw = parsePathSeg

-- | Parses and discards a single path segment if it satisfies the given predicate function
--
-- >>> import qualified Data.Text
-- >>> isValidProductID = \pid -> Data.Text.length pid > 5
-- >>> :{
-- parser = do
--   get
--   pathSeg "product"
--   pathSegWith isValidProductID
--   respond ok
-- :}
--
-- >>> result1 <- testParserIO parser (TestRequest "GET" [] "/product/242301" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testParserIO parser (TestRequest "GET" [] "/product/5641" "")
-- >>> assertFailure isSkip result2
-- True
pathSegWith :: forall m. MonadOkapi m => (Text.Text -> Bool) -> m ()
pathSegWith predicate = do
  pathSeg <- parsePathSeg
  if predicate pathSeg
    then pure ()
    else skip

-- | Parses all the remaining path segments of a request
pathWildcard :: forall m. MonadOkapi m => m (NonEmpty.NonEmpty Text.Text)
pathWildcard = do
  segs <- some pathParamRaw
  case segs of
    [] -> skip
    _ -> pure $ NonEmpty.fromList segs

-- QUERY HELPERS

-- | Parses the value of a query parameter with the given type and name
--
-- >>> :set -XTypeApplications
-- >>> import qualified Data.ByteString.Lazy as LBS
-- >>> import qualified Data.ByteString.Char8 as C8
-- >>> showLBS = LBS.fromStrict . C8.pack . show
-- >>> :{
-- parser = do
--   get
--   pathSeg "product"
--   minQty <- queryParam @Int "min_qty"
--   respond $ setBodyRaw (showLBS $ minQty + 3) $ ok
-- :}
--
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/product?min_qty=2" "")
-- >>> assertResponse is200 result
-- True
-- >>> assertResponse (hasBodyRaw "5") result
-- True
queryParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  case queryItemValue of
    QueryFlag -> skip
    QueryParam valueText -> maybe skip pure (Web.parseQueryParamMaybe valueText)

-- | Parses the value of a query parameter as raw @Text@.
-- Use this instead of @queryParam@ if you want to process the query parameter yourself
--
-- >>> data Bit = Zero | One
-- >>> :{
-- parseBit text =
--   case text of
--     "b0" -> Just Zero
--     "b1" -> Just One
--     _    -> Nothing
-- :}
--
-- >>> :{
-- parser = do
--   get
--   path ["flip", "my", "bit"]
--   bitRaw <- queryParamRaw "value"
--   case parseBit bitRaw of
--     Just Zero -> respond $ setBodyRaw "1" $ ok
--     Just One  -> respond $ setBodyRaw "0" $ ok
--     Nothing   -> throw _500
-- :}
--
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/flip/my/bit?value=b0" "")
-- >>> assertResponse (hasBodyRaw "1") result
-- True
queryParamRaw :: forall m. MonadOkapi m => Text.Text -> m Text.Text
queryParamRaw queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  case queryItemValue of
    QueryFlag -> skip
    QueryParam raw -> pure raw

-- | Test for the existance of a query flag
--
-- >>> :{
-- parser = do
--   get
--   pathSeg "users"
--   isAdmin <- queryFlag "admin"
--   respond $
--     if isAdmin
--       then json ["Derek", "Alice"] $ ok
--       else json ["Derek", "Alice", "Bob", "Casey", "Alex", "Larry"] $ ok
-- :}
--
-- >>> result1 <- testParserIO parser (TestRequest "GET" [] "/users?admin" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\"]") result1
-- True
-- >>> result2 <- testParserIO parser (TestRequest "GET" [] "/users?admin=foobarbaz" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\"]") result2
-- True
-- >>> result3 <- testParserIO parser (TestRequest "GET" [] "/users" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\",\"Bob\",\"Casey\",\"Alex\",\"Larry\"]") result3
-- True
queryFlag :: forall a m. MonadOkapi m => Text.Text -> m Bool
queryFlag queryItemName = do
  maybeQueryItem <- optional $ parseQueryItem queryItemName
  pure $ case maybeQueryItem of
    Nothing -> False
    Just _ -> True

-- HEADER HELPERS

basicAuth :: forall m. MonadOkapi m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case decodeBase64 encodedCreds of
        Left _ -> skip
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> skip
    _ -> skip

-- TODO: cookie :: forall m. MonadOkapi m => Cookie

cookies :: forall m. MonadOkapi m => m Cookies
cookies = do
  cookiesValue <- header "Cookie"
  pure $ Cookie.parseCookiesText cookiesValue

-- TODO: Any checks required??
header :: forall m. MonadOkapi m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  (_headerName, headerValue) <- parseHeader headerName
  pure headerValue

-- TODO: headers :: forall m. MonadOkapi m => m Headers

-- BODY HELPERS

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyJSON :: forall a m. (MonadOkapi m, Aeson.FromJSON a) => m a
bodyJSON = do
  body <- bodyRaw
  maybe skip pure (Aeson.decode body)

bodyForm :: forall a m. (MonadOkapi m, Web.FromForm a) => m a
bodyForm = do
  body <- bodyRaw
  maybe skip pure (eitherToMaybe $ Web.urlDecodeAsForm body)
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- TODO: bodyFile functions for file uploads to server?
bodyRaw :: forall m. MonadOkapi m => m LBS.ByteString
bodyRaw = parseBody

-- Response helpers

respond :: forall m. MonadOkapi m => Response -> m Response
respond response = do
  check1 <- methodParsed
  check2 <- pathParsed
  -- check3 <- queryParsed
  if check1 && check2 then return response else skip

-- TODO: add end parser similar to <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>

-- Error HELPERS

skip :: forall a m. MonadOkapi m => m a
skip = Except.throwError Skip

throw :: forall a m. MonadOkapi m => Response -> m a
throw = Except.throwError . Error

(<!>) :: forall a m. MonadOkapi m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: forall a m. MonadOkapi m => Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

optionalThrow :: forall a m. MonadOkapi m => m a -> m (Maybe a)
optionalThrow parser = (Just <$> parser) <!> pure Nothing

optionThrow :: forall a m. MonadOkapi m => a -> m a -> m a
optionThrow value parser = do
  mbValue <- optionalThrow parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'

-- State Checks

methodParsed :: MonadOkapi m => m Bool
methodParsed = State.gets stateRequestMethodParsed

pathParsed :: MonadOkapi m => m Bool
pathParsed = State.gets (Prelude.null . requestPath . stateRequest)

queryParsed :: MonadOkapi m => m Bool
queryParsed = State.gets (Prelude.null . requestQuery . stateRequest)

headersParsed :: MonadOkapi m => m Bool
headersParsed = State.gets (Prelude.null . requestHeaders . stateRequest)

bodyParsed :: MonadOkapi m => m Bool
bodyParsed = State.gets stateRequestBodyParsed

-- PRIMITIVE PARSERS (BELOW IS INTERNAL)

parseMethod :: MonadOkapi m => m HTTP.Method
parseMethod = do
  isMethodParsed <- methodParsed
  if isMethodParsed
    then skip
    else do
      method <- State.gets (requestMethod . stateRequest)
      State.modify (\state -> state {stateRequestMethodParsed = True})
      pure method

parsePath :: MonadOkapi m => m [Text.Text]
parsePath = some parsePathSeg

parsePathSeg :: MonadOkapi m => m Text.Text
parsePathSeg = do
  maybePathSeg <- State.gets (safeHead . requestPath . stateRequest)
  case maybePathSeg of
    Nothing -> skip
    Just pathSeg -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestPath = Prelude.drop 1 $ requestPath $ stateRequest state}})
      pure pathSeg
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

parseQueryItem :: MonadOkapi m => Text.Text -> m QueryItem
parseQueryItem queryItemName = do
  maybeQueryItem <- State.gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName') . requestQuery . stateRequest)
  case maybeQueryItem of
    Nothing -> skip
    Just queryItem -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = List.delete queryItem $ requestQuery $ stateRequest state}})
      pure queryItem

parseQuery :: MonadOkapi m => m Query
parseQuery = do
  query <- State.gets (requestQuery . stateRequest)
  State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = []}})
  pure query

parseHeaders :: MonadOkapi m => m Headers
parseHeaders = do
  headers <- State.gets (requestHeaders . stateRequest)
  State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = []}})
  pure headers

parseHeader :: MonadOkapi m => HTTP.HeaderName -> m Header
parseHeader headerName = do
  maybeHeader <- State.gets (Foldable.find (\(headerName', _) -> headerName == headerName') . requestHeaders . stateRequest)
  case maybeHeader of
    Nothing -> skip
    Just header -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = List.delete header $ requestHeaders $ stateRequest state}})
      pure header

parseBody :: forall m. MonadOkapi m => m LBS.ByteString
parseBody = do
  isBodyParsed <- bodyParsed
  if isBodyParsed
    then skip
    else do
      body <- State.gets (requestBody . stateRequest)
      State.modify (\state -> state {stateRequestBodyParsed = True})
      pure body

parseRequest :: MonadOkapi m => m Request
parseRequest = Request <$> parseMethod <*> parsePath <*> parseQuery <*> parseBody <*> parseHeaders

match :: MonadOkapi m => (Request -> m Response) -> m Response
match matcher = parseRequest >>= matcher

-- TODO: Probably don't need??? I don't think so after some thought
{-
routeToFile :: MonadOkapi m => Status -> Headers -> m Response
routeToFile status headers = do
  nonEmptyPath <- pathWildcard
  let filePath = nonEmptyPathToFilePath nonEmptyPath
  respond $ Response status headers $ ResponseBodyFile filePath
  where
    nonEmptyPathToFilePath :: NonEmpty Text -> FilePath
    nonEmptyPathToFilePath (base :| path) = unpack $ base <> Text.intercalate "/" path

static :: MonadOkapi m => [Text] ->
static
-}

lookupQuery :: MonadOkapi m => Text.Text -> Query -> m QueryValue
lookupQuery name query = maybe skip pure (List.lookup name query)

lookupHeaders :: MonadOkapi m => HeaderName -> Headers -> m BS.ByteString
lookupHeaders name headers = maybe skip pure (List.lookup name headers)

lookupForm :: (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> Body -> m a
lookupForm = undefined

{-
TODO: HTTPDataStore? Not really needed because you can just pass data normally or store in own monad.
One benefit is that the data is available to all sub-branches without explicitly passing them to every sub-branch.

-- This data structure should be hidden from user
data HTTPDataStore = HTTPDataStore
  { pathStore   :: (Map Text Text)
  , queryStore  :: (Map Text Text)
  , headerStore :: (Map Text Text)
  }

-- Can only store parsed information
storePathParam :: forall a. FromHttpApiData a => Text -> Okapi a
storePathParam = ...

storeQueryParam :: ... => Text -> Okapi a

storeHeader :: ... => Text -> Okapi a

-- Can fail on Map lookup and data conversion
findPathParam :: forall a. FromHttpApiData a => Okapi a
-}

-- PATTERNS.HS

pattern GET :: Method
pattern GET = "GET"

pattern POST :: Method
pattern POST = "POST"

pattern DELETE :: Method
pattern DELETE = "DELETE"

pattern OTHER :: Method -> Method
pattern OTHER method <-
  method
  where
    OTHER method = method

pattern PathParam :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> Text.Text
pattern PathParam param <-
  (parseUrlPiece -> Right param)
  where
    PathParam param = toUrlPiece param

pattern HasQueryParam :: Web.FromHttpApiData a => a -> Maybe QueryValue
pattern HasQueryParam value <- Just (QueryParam (parseQueryParam -> Right value))

pattern HasQueryFlag :: Maybe QueryValue
pattern HasQueryFlag <- Just QueryFlag

viewQuery :: Text.Text -> Query -> (Maybe QueryValue, Query)
viewQuery name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

viewHeaders :: HeaderName -> Headers -> (Maybe BS.ByteString, Headers)
viewHeaders name headers = case List.lookup name headers of
  Nothing -> (Nothing, headers)
  Just value -> (Just value, List.delete (name, value) headers)

-- RESPONSE.HS

-- BASE RESPONSES

ok :: Response
ok =
  let responseStatus = 200
      responseHeaders = []
      responseBody = ResponseBodyRaw "OK"
   in Response {..}

notFound :: Response
notFound =
  let responseStatus = 404
      responseHeaders = []
      responseBody = ResponseBodyRaw "Not Found"
   in Response {..}

redirect :: Status -> URL -> Response
redirect status (URL url) =
  let responseStatus = status
      responseHeaders = [("Location", encodeUtf8 url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}

-- RESPONSE BODY MODIFIERS

plaintext :: Text.Text -> Response -> Response
plaintext text response =
  response
    & setHeader ("Content-Type", "text/plain")
    & setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

html :: LBS.ByteString -> Response -> Response
html htmlRaw response =
  response
    & setBody (ResponseBodyRaw htmlRaw)
    & setHeader ("Content-Type", "text/html")

json :: forall a. Aeson.ToJSON a => a -> Response -> Response
json value response =
  response
    & setHeader ("Content-Type", "application/json")
    & setBodyRaw (Aeson.encode value)

setBodyFile :: FilePath -> Response -> Response
setBodyFile path = setBody (ResponseBodyFile path) -- TODO: setHeader???

setBodyEventSource :: EventSource -> Response -> Response
setBodyEventSource source response =
  response
    & setBody (ResponseBodyEventSource source)

-- RESPONSE SETTERS

setStatus :: Status -> Response -> Response
setStatus status response = response {responseStatus = status}

setHeaders :: Headers -> Response -> Response
setHeaders headers response = response {responseHeaders = headers}

-- TODO: setCookie

setHeader :: Header -> Response -> Response
setHeader header response@Response {..} =
  response {responseHeaders = update header responseHeaders}
  where
    update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    update pair [] = [pair]
    update pair@(key, value) (pair'@(key', value') : ps) =
      if key == key'
        then pair : ps
        else pair' : update pair ps

setBody :: ResponseBody -> Response -> Response
setBody body response = response {responseBody = body}

setBodyRaw :: LBS.ByteString -> Response -> Response
setBodyRaw bodyRaw = setBody (ResponseBodyRaw bodyRaw)
