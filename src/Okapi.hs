{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Okapi is a micro web framework.
module Okapi
  ( -- * Parsing
    -- $parsers
    MonadOkapi,
    OkapiT (..),
    Failure (..),
    State (..),
    Request,
    Method,
    Path,
    Query,
    QueryItem (..),
    QueryValue (..),
    Body,
    Headers,
    Header,
    HeaderName,
    Okapi.Cookie,
    Crumb,

    -- ** Request Parsers
    request,
    requestEnd,

    -- *** Method Parsers
    -- $methodParsers
    method,
    methodGET,
    methodPOST,
    methodHEAD,
    methodPUT,
    methodPATCH,
    methodDELETE,
    methodOPTIONS,
    methodTRACE,
    methodCONNECT,
    methodEnd,

    -- *** Path Parsers
    -- $pathParsers
    path,
    pathParam,
    pathEnd,

    -- *** Query Parsers
    -- $queryParsers
    query,
    queryValue,
    queryFlag,
    queryParam,
    queryParamList,
    queryEnd,

    -- *** Body Parsers
    -- $bodyParsers
    body,
    bodyJSON,
    bodyURLEncoded,
    bodyEnd,

    -- *** Header Parsers
    -- $headerParsers
    headers,
    header,
    basicAuth,
    headersEnd,
    cookie,
    cookieCrumb,
    cookieEnd,

    -- ** Vault Parsers
    -- $vaultParsers
    vaultLookup,
    vaultInsert,
    vaultDelete,
    vaultAdjust,
    vaultWipe,

    -- ** Combinators
    -- $combinators
    is,
    satisfies,
    Okapi.look,
    module Combinators,

    -- ** Failure
    -- $failure
    next,
    throw,
    (<!>),
    guardThrow,

    -- * Responding
    -- $responding
    Handler (..),
    Response (..),
    Status,
    ResponseBody (..),

    -- ** Values
    ok,
    notFound,
    redirect,
    forbidden,
    internalServerError,

    -- ** Setters
    setStatus,
    setHeaders,
    setHeader,
    addHeader,
    addSetCookie,
    setBody,
    setBodyRaw,
    setBodyFile,
    setBodyEventSource,
    setPlaintext,
    setHTML,
    setJSON,

    -- ** Special
    static,

    -- * Middleware
    -- $middleware
    Middleware (..),
    applyMiddlewares,
    scope,
    clearHeadersMiddleware,
    prefixPathMiddleware,

    -- * Routing
    -- $routing
    Router (..),
    route,
    pattern PathParam,
    pattern GET,
    pattern POST,
    pattern DELETE,
    pattern PUT,
    pattern PATCH,
    pattern IsQueryParam,
    pattern HasQueryFlag,
    viewQuery,
    viewQueryParam,

    -- * Relative URLs
    -- $relativeURLs
    RelURL (..),
    renderRelURL,
    renderPath,
    renderQuery,
    parseRelURL,

    -- * Testing
    -- $testing
    test,
    testPure,
    testIO,
    assert,
    assert200,
    assert404,
    assert500,

    -- * WAI
    -- $wai
    run,
    serve,
    serveTLS,
    serveWebsockets,
    serveWebsocketsTLS,
    app,
    websocketsApp,
    testRunSession,
    testWithSession,
    testRequest,

    -- * Utilities

    -- ** Server Sent Events
    -- $serverSentEvents
    Event (..),
    ToSSE (..),
    EventSource,
    newEventSource,
    sendValue,
    sendEvent,

    -- ** Sessions
    Session (..),
    HasSession (..),
    session,
    sessionLookup,
    sessionInsert,
    sessionDelete,
    sessionClear,
    withSession,
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
import qualified Network.Wai.Handler.Warp as WAI hiding (FileInfo(..))
import qualified Network.Wai.Handler.WarpTLS as WAI
import qualified Network.Wai.Handler.WebSockets as WAI
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.Wai.Internal as WAI
import qualified Network.Wai.Middleware.Gzip as WAI
import qualified Network.Wai.Parse as WAI
import qualified Network.Wai.Test as WAI
import qualified Network.WebSockets as WebSockets
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

-- $parserTypes
--
-- The types are as follows

-- | A type constraint representing monads that have the ability to parse an HTTP request.
type MonadOkapi m =
  ( Functor m,
    Applicative m,
    Applicative.Alternative m,
    Monad m,
    Monad.MonadPlus m,
    Except.MonadError Failure m,
    State.MonadState State m
  )

-- | A concrete implementation of the @MonadOkapi@ type constraint.
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

instance IO.MonadIO m => IO.MonadIO (OkapiT m) where
  liftIO = Morph.lift . IO.liftIO 

instance Morph.MonadTrans OkapiT where
  lift :: Monad m => m a -> OkapiT m a
  lift action = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor OkapiT where
  hoist :: Monad m => (forall a. m a -> n a) -> OkapiT m b -> OkapiT n b
  hoist nat okapiT = OkapiT . Except.ExceptT . State.StateT $ (nat . State.runStateT (Except.runExceptT $ unOkapiT okapiT))

-- | Represents the state of a parser. Set on every request to the Okapi server.
data State = State
  { stateRequest :: Request,
    stateVault :: Vault.Vault
  }

-- | Represents the HTTP request being parsed.
data Request = Request
  { requestMethod :: Method,
    requestPath :: Path,
    requestQuery :: Query,
    requestBody :: Body,
    requestHeaders :: Headers
  }
  deriving (Eq, Show)

type Method = Maybe BS.ByteString

type Path = [Text.Text]

type Query = [QueryItem]

type QueryItem = (Text.Text, QueryValue)

data QueryValue = QueryParam Text.Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

data Body = BodyRaw LBS.ByteString
  | BodyMultipart ([WAI.Param], [WAI.File LBS.ByteString])
  deriving (Eq, Show)

type Headers = [Header]

type Header = (HeaderName, BS.ByteString)

type HeaderName = HTTP.HeaderName

type Cookie = [Crumb]

type Crumb = (BS.ByteString, BS.ByteString)

-- $parsers
--
-- These are the parsers that you'll use to build you own app.

-- | Parses the entire request.
request :: MonadOkapi m => m Request
request = Request <$> method <*> path <*> query <*> body <*> headers

requestEnd :: MonadOkapi m => m ()
requestEnd = do
  methodEnd
  pathEnd
  queryEnd
  headersEnd
  bodyEnd

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request method.

method :: MonadOkapi m => m Method
method = do
  maybeMethod <- State.gets (requestMethod . stateRequest)
  case maybeMethod of
    Nothing -> pure Nothing
    method'@(Just _) -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestMethod = Nothing}})
      pure method'

methodGET :: MonadOkapi m => m ()
methodGET = is method $ Just HTTP.methodGet

methodPOST :: MonadOkapi m => m ()
methodPOST = is method $ Just HTTP.methodPost

methodHEAD :: MonadOkapi m => m ()
methodHEAD = is method $ Just HTTP.methodHead

methodPUT :: MonadOkapi m => m ()
methodPUT = is method $ Just HTTP.methodPut

methodDELETE :: MonadOkapi m => m ()
methodDELETE = is method $ Just HTTP.methodDelete

methodTRACE :: MonadOkapi m => m ()
methodTRACE = is method $ Just HTTP.methodTrace

methodCONNECT :: MonadOkapi m => m ()
methodCONNECT = is method $ Just HTTP.methodConnect

methodOPTIONS :: MonadOkapi m => m ()
methodOPTIONS = is method $ Just HTTP.methodOptions

methodPATCH :: MonadOkapi m => m ()
methodPATCH = is method $ Just HTTP.methodPatch

methodEnd :: MonadOkapi m => m ()
methodEnd = do
  maybeMethod <- Combinators.optional method
  case maybeMethod of
    Nothing -> pure ()
    Just _ -> next

-- $pathParsers
--
-- These are the path parsers.

-- | Parses and discards mutiple path segments matching the values and order of the given @[Text]@ value
path :: MonadOkapi m => m [Text.Text]
path = Combinators.many pathParam

-- | Parses and discards a single path segment matching the given @Text@ value
pathParam :: (Web.FromHttpApiData a, MonadOkapi m) => m a
pathParam = do
  maybePathSeg <- State.gets (safeHead . requestPath . stateRequest)
  case maybePathSeg of
    Nothing -> next
    Just pathSeg -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestPath = Prelude.drop 1 $ requestPath $ stateRequest state}})
      maybe next pure (Web.parseUrlPieceMaybe pathSeg)
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
pathEnd :: MonadOkapi m => m ()
pathEnd = do
  currentPath <- path
  if List.null currentPath
    then pure ()
    else next

-- $queryParsers
--
-- These are the query parsers.

query :: MonadOkapi m => m Query
query = do
  query <- State.gets (requestQuery . stateRequest)
  State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = []}})
  pure query

queryValue :: MonadOkapi m => Text.Text -> m QueryValue
queryValue queryItemName = do
  maybeQueryItem <- State.gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName') . requestQuery . stateRequest)
  case maybeQueryItem of
    Nothing -> next
    Just queryItem@(_, queryValue) -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = List.delete queryItem $ requestQuery $ stateRequest state}})
      pure queryValue

-- | Parses the value of a query parameter with the given type and name
queryParam :: (Web.FromHttpApiData a, MonadOkapi m) => Text.Text -> m a
queryParam queryItemName = do
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    QueryFlag -> next
    QueryParam valueText -> maybe next pure (Web.parseQueryParamMaybe valueText)

-- | Test for the existance of a query flag
queryFlag :: MonadOkapi m => Text.Text -> m ()
queryFlag queryItemName = do
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    QueryFlag -> pure ()
    _ -> next

queryParamList :: (Web.FromHttpApiData a, MonadOkapi m) => Text.Text -> m (NonEmpty.NonEmpty a)
queryParamList = Combinators.NonEmpty.some . queryParam

queryEnd :: MonadOkapi m => m ()
queryEnd = do
  currentQuery <- query
  if List.null currentQuery
    then pure ()
    else next

-- $bodyParsers

-- | For getting the raw body of the request.
body :: MonadOkapi m => m Body
body = do
  currentBody <- State.gets (requestBody . stateRequest)
  case currentBody of
    BodyRaw (LBS.null -> True) -> next
    BodyMultipart ([], []) -> next
    BodyRaw _ -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestBody = BodyRaw ""}})
      pure currentBody
    BodyMultipart _ -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestBody = BodyMultipart ([], [])}})
      pure currentBody

-- | Parse request body as JSON
bodyJSON :: (Aeson.FromJSON a, MonadOkapi m) => m a
bodyJSON = do
  body' <- body
  case body' of
    BodyRaw lbs -> maybe next pure (Aeson.decode lbs)
    BodyMultipart _ -> next

-- | Parse URLEncoded form parameters from request body
bodyURLEncoded :: (Web.FromForm a, MonadOkapi m) => m a
bodyURLEncoded = do
  body' <- body
  case body' of
    BodyRaw lbs -> maybe next pure (eitherToMaybe $ Web.urlDecodeAsForm lbs)
    BodyMultipart _ -> next
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- | Parse multipart form data from request body
bodyMultipart :: MonadOkapi m => m ([WAI.Param], [WAI.File LBS.ByteString])
bodyMultipart = do
  body' <- body
  case body' of
    BodyRaw _ -> next
    BodyMultipart formData -> pure formData

bodyXML = undefined

-- | Parse a single form parameter
formParam :: forall a m. (Web.FromHttpApiData a, MonadOkapi m) => BS.ByteString -> m a
formParam paramName = do
  body' <- body
  case body' of
    BodyRaw lbs -> do
      case Web.urlDecodeParams lbs of
        Left _ -> next
        Right params ->
          case lookup (Text.decodeUtf8 paramName) params of
            Nothing -> next
            Just paramValue -> do
              paramValue' <- maybe next pure (Web.parseQueryParamMaybe paramValue)
              let newParams = List.delete ((Text.decodeUtf8 paramName), paramValue) params
              State.modify (\state -> state {stateRequest = (stateRequest state) {requestBody = BodyRaw $ Web.urlEncodeParams newParams}})
              pure paramValue'
    BodyMultipart (params, files) -> do
      case lookup paramName params of
        Nothing -> next
        Just paramValue -> do
          paramValue' <- maybe next pure (Web.parseQueryParamMaybe $ Text.decodeUtf8 paramValue)
          let newParams = List.delete (paramName, paramValue) params
          State.modify (\state -> state {stateRequest = (stateRequest state) {requestBody = BodyMultipart (newParams, files)}})
          pure paramValue'

formParamList :: forall a m. (Web.FromHttpApiData a, MonadOkapi m) => BS.ByteString -> m (NonEmpty.NonEmpty a)
formParamList = Combinators.NonEmpty.some . formParam

-- | Parse a single form file
formFile :: MonadOkapi m => BS.ByteString -> m (WAI.FileInfo LBS.ByteString)
formFile fileName = do
  body' <- body
  case body' of
    BodyRaw _ -> next
    BodyMultipart (params, files) -> do
      case lookupFile fileName files of
        Nothing -> next
        Just fileInfo -> pure fileInfo
  where
    lookupFile :: BS.ByteString -> [WAI.File LBS.ByteString] -> Maybe (WAI.FileInfo LBS.ByteString)
    lookupFile = undefined

    deleteFile :: BS.ByteString -> [WAI.File LBS.ByteString] -> [WAI.File LBS.ByteString]
    deleteFile = undefined

bodyEnd :: MonadOkapi m => m ()
bodyEnd = do
  currentBody <- State.gets (requestBody . stateRequest)
  case currentBody of
    BodyRaw (LBS.null -> True) -> pure ()
    BodyMultipart ([], []) -> pure ()
    _ -> next

-- $headerParsers
--
-- These are header parsers.

headers :: MonadOkapi m => m Headers
headers = do
  headers <- State.gets (requestHeaders . stateRequest)
  State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = []}})
  pure headers

header :: MonadOkapi m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  maybeHeader <- State.gets (Foldable.find (\(headerName', _) -> headerName == headerName') . requestHeaders . stateRequest)
  case maybeHeader of
    Nothing -> next
    Just header@(_, headerValue) -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = List.delete header $ requestHeaders $ stateRequest state}})
      pure headerValue

headersEnd :: MonadOkapi m => m ()
headersEnd = do
  currentHeaders <- headers
  if List.null currentHeaders
    then pure ()
    else next

cookie :: MonadOkapi m => m Cookie
cookie = do
  cookieValue <- header "Cookie"
  pure $ Web.parseCookies cookieValue

cookieCrumb :: MonadOkapi m => BS.ByteString -> m BS.ByteString
cookieCrumb name = do
  cookieValue <- cookie
  case List.lookup name cookieValue of
    Nothing -> next
    Just crumbValue -> do
      let crumb = (name, crumbValue)
      -- TODO: Needs testing to see if state is restored properly
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete crumb cookieValue) : requestHeaders (stateRequest state)}})
      pure crumbValue

cookieEnd :: MonadOkapi m => m ()
cookieEnd = do
  currentCookie <- cookie
  if List.null currentCookie
    then pure ()
    else next

basicAuth :: MonadOkapi m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case Text.decodeBase64 encodedCreds of
        Left _ -> next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> next
    _ -> next

-- $vaultParsers

vaultLookup :: MonadOkapi m => Vault.Key a -> m a
vaultLookup key = do
  vault <- State.gets stateVault
  maybe next pure (Vault.lookup key vault)

vaultInsert :: MonadOkapi m => Vault.Key a -> a -> m ()
vaultInsert key value = do
  vault <- State.gets stateVault
  State.modify (\state -> state {stateVault = Vault.insert key value vault})

vaultDelete :: MonadOkapi m => Vault.Key a -> m ()
vaultDelete key = do
  vault <- State.gets stateVault
  State.modify (\state -> state {stateVault = Vault.delete key vault})

vaultAdjust :: MonadOkapi m => (a -> a) -> Vault.Key a -> m ()
vaultAdjust adjuster key = do
  vault <- State.gets stateVault
  State.modify (\state -> state {stateVault = Vault.adjust adjuster key vault})

vaultWipe :: MonadOkapi m => m ()
vaultWipe = State.modify (\state -> state {stateVault = Vault.empty})

-- $combinators

is :: (Eq a, MonadOkapi m) => m a -> a -> m ()
is action desired = satisfies action (desired ==)

satisfies :: (Eq a, MonadOkapi m) => m a -> (a -> Bool) -> m ()
satisfies action predicate = do
  value <- action
  if predicate value
    then pure ()
    else next

-- | Parses without modifying the state, even if it succeeds.
look :: MonadOkapi m => m a -> m a
look parser = do
  state <- State.get
  result <- parser
  State.put state
  pure result

-- $error

-- | Represents the two variants of failure that can occur when parsing a HTTP request.
data Failure = Skip | Error Response

instance Show Failure where
  show Skip = "Skipped"
  show (Error _) = "Error returned"

next :: MonadOkapi m => m a
next = Except.throwError Skip

throw :: MonadOkapi m => Response -> m a
throw = Except.throwError . Error

(<!>) :: MonadOkapi m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: MonadOkapi m => Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

-- $response

-- | Represents monadic actions that return a @Response@, for some @m@.
type Handler m = m Response

-- | Represents HTTP responses that can be returned by a parser.
data Response = Response
  { responseStatus :: Status,
    responseHeaders :: Headers,
    responseBody :: ResponseBody
  }

type Status = Natural.Natural

-- | Represents the body of an HTTP response.
data ResponseBody
  = ResponseBodyRaw LBS.ByteString
  | ResponseBodyFile FilePath
  | ResponseBodyEventSource EventSource

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

redirect :: Status -> Text.Text -> Response
redirect status url =
  let responseStatus = status
      responseHeaders = [("Location", Text.encodeUtf8 url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}

forbidden :: Response
forbidden =
  let responseStatus = 403
      responseHeaders = []
      responseBody = ResponseBodyRaw "Forbidden"
   in Response {..}

internalServerError :: Response
internalServerError =
  let responseStatus = 500
      responseHeaders = []
      responseBody = ResponseBodyRaw "Internal Server Error"
   in Response {..}

-- RESPONSE SETTERS

setStatus :: Status -> Response -> Response
setStatus status response = response {responseStatus = status}

setHeaders :: Headers -> Response -> Response
setHeaders headers response = response {responseHeaders = headers}

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

addHeader :: Header -> Response -> Response
addHeader header response = response {responseHeaders = header : responseHeaders response}

addSetCookie :: (BS.ByteString, BS.ByteString) -> Response -> Response
addSetCookie (key, value) response =
  let setCookieValue =
        LBS.toStrict $
          Builder.toLazyByteString $
            Web.renderSetCookie $
              Web.defaultSetCookie -- TODO: Check that using default here is okay
                { Web.setCookieName = key,
                  Web.setCookieValue = value,
                  Web.setCookiePath = Just "/"
                }
   in addHeader ("Set-Cookie", setCookieValue) response

setBody :: ResponseBody -> Response -> Response
setBody body response = response {responseBody = body}

setBodyRaw :: LBS.ByteString -> Response -> Response
setBodyRaw bodyRaw = setBody (ResponseBodyRaw bodyRaw)

setBodyFile :: FilePath -> Response -> Response
setBodyFile path = setBody (ResponseBodyFile path) -- TODO: setHeader???

setBodyEventSource :: EventSource -> Response -> Response
setBodyEventSource source response =
  response
    Function.& setBody (ResponseBodyEventSource source)

setPlaintext :: Text.Text -> Response -> Response
setPlaintext text response =
  response
    Function.& setHeader ("Content-Type", "text/plain")
    Function.& setBodyRaw (LBS.fromStrict . Text.encodeUtf8 $ text)

setHTML :: LBS.ByteString -> Response -> Response
setHTML htmlRaw response =
  response
    Function.& setBody (ResponseBodyRaw htmlRaw)
    Function.& setHeader ("Content-Type", "text/html")

setJSON :: Aeson.ToJSON a => a -> Response -> Response
setJSON value response =
  response
    Function.& setHeader ("Content-Type", "application/json")
    Function.& setBodyRaw (Aeson.encode value)

static :: MonadOkapi m => Handler m
static = do
  filePathText <- Text.intercalate "/" <$> path
  let filePath = Text.unpack filePathText
  ok Function.& setBodyFile filePath Function.& pure

-- $serverSentEvents

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

newEventSource :: IO EventSource
newEventSource = Unagi.newChan

sendValue :: ToSSE a => EventSource -> a -> IO ()
sendValue (inChan, _outChan) = Unagi.writeChan inChan . toSSE

sendEvent :: EventSource -> Event -> IO ()
sendEvent (inChan, _outChan) = Unagi.writeChan inChan

-- BELOW IS INTERNAL

eventSourceAppUnagiChan :: EventSource -> WAI.Application
eventSourceAppUnagiChan (inChan, _outChan) req sendResponse = do
  outChan <- IO.liftIO $ Unagi.dupChan inChan
  eventSourceAppIO (eventToServerEvent <$> Unagi.readChan outChan) req sendResponse

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
            Nothing -> pure ()
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

-- $wai
--
-- These functions are for interfacing with WAI (Web Application Interface).

run :: Monad m => (forall a. m a -> IO a) -> OkapiT m Response -> IO ()
run = serve 3000 notFound

serve ::
  Monad m =>
  -- | Port
  Int ->
  -- | Default Response
  Response ->
  -- | Monad unlift function
  (forall a. m a -> IO a) ->
  -- | Parser
  OkapiT m Response ->
  IO ()
serve port defaultResponse hoister okapiT = WAI.run port $ app defaultResponse hoister okapiT

serveTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  Response ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO ()
serveTLS tlsSettings settings defaultResponse hoister okapiT = WAI.runTLS tlsSettings settings $ app defaultResponse hoister okapiT

serveWebsockets ::
  Monad m =>
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  Int ->
  Response ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO ()
serveWebsockets connSettings serverApp port defaultResponse hoister okapiT = WAI.run port $ websocketsApp connSettings serverApp defaultResponse hoister okapiT

serveWebsocketsTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  Response ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO ()
serveWebsocketsTLS tlsSettings settings connSettings serverApp defaultResponse hoister okapiT = WAI.runTLS tlsSettings settings $ websocketsApp connSettings serverApp defaultResponse hoister okapiT

-- | Turns a parser into a WAI application
app ::
  Monad m =>
  -- | The default response to pure if parser fails
  Response ->
  -- | Function for "unlifting" monad inside @OkapiT@ to @IO@ monad
  (forall a. m a -> IO a) ->
  -- | The parser used to equals the request
  OkapiT m Response ->
  WAI.Application
app defaultResponse hoister okapiT waiRequest respond = do
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
      requestBody <- case lookup "Content-Type" $ WAI.requestHeaders waiRequest of
          Just "multipart/form-data" -> BodyMultipart <$> WAI.parseRequestBody WAI.lbsBackEnd waiRequest
          _ -> BodyRaw <$> WAI.strictRequestBody waiRequest -- TODO: Use lazy request body???
      let requestMethod = Just $ WAI.requestMethod waiRequest
          requestPath = WAI.pathInfo waiRequest
          requestQuery = map (\case (name, Nothing) -> (name, QueryFlag); (name, Just txt) -> (name, QueryParam txt)) $ HTTP.queryToQueryText $ WAI.queryString waiRequest
          requestHeaders = WAI.requestHeaders waiRequest
          stateRequest = Request {..}
          stateVault = WAI.vault waiRequest
      pure State {..}

-- | Turns a parsers into a WAI application with WebSocket functionality
-- See __ for information on how to create a WebSocket server
websocketsApp ::
  Monad m =>
  -- | Connection options configuration for the WebSocket server
  WebSockets.ConnectionOptions ->
  -- | The server to use for handling WebSocket connections
  WebSockets.ServerApp ->
  Response ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  WAI.Application
websocketsApp connSettings serverApp defaultResponse hoister okapiT =
  let backupApp = app defaultResponse hoister okapiT
   in WebSockets.websocketsOr connSettings serverApp backupApp

-- $middleware
--
-- Middlewares allow you to modify the behavior of Okapi handlers.
-- Middlewares are functions that take a handler and return another handler.
-- Middlewares can be composed with the fish operator @>=>@.
--
-- @
--  clearHeadersMiddleware >=> pathPrefix ["jello"] :: forall m. Middleware m
-- @

-- | A middleware takes an action that returns a @Response@ and can modify the action in various ways
type Middleware m = Handler m -> Handler m

applyMiddlewares :: MonadOkapi m => [Middleware m] -> Middleware m
applyMiddlewares middlewares handler =
  List.foldl (\handler middleware -> middleware handler) handler middlewares

-- TODO: Is this needed? Idea taken from OCaml Dream framework

scope :: MonadOkapi m => Path -> [Middleware m] -> Middleware m
scope prefix middlewares handler = path `is` prefix >> applyMiddlewares middlewares handler

clearHeadersMiddleware :: MonadOkapi m => Middleware m
clearHeadersMiddleware handler = setHeaders [] <$> handler

prefixPathMiddleware :: MonadOkapi m => Path -> Middleware m
prefixPathMiddleware prefix handler = path `is` prefix >> handler

-- $routing
--
-- Okapi implements routes and type-safe relative URLs using bidirectional pattern synonyms and view patterns.
-- Routing can be extended to dispatch on any property of the request, including method, path, query, headers, and even body.
-- By default, Okapi provides a @route@ function for dispatching on the path of the request.

type Router m a =
  -- | Parser for dispatcher
  m a ->
  -- | Dispatches parser result to the correct handler
  (a -> Handler m) ->
  Handler m

route :: MonadOkapi m => Router m a
route parser dispatcher = parser >>= dispatcher

-- $patterns

pattern GET :: Method
pattern GET = Just "GET"

pattern POST :: Method
pattern POST = Just "POST"

pattern PATCH :: Method
pattern PATCH = Just "PATCH"

pattern DELETE :: Method
pattern DELETE = Just "DELETE"

pattern PUT :: Method
pattern PUT = Just "PUT"

pattern PathParam :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> Text.Text
pattern PathParam param <-
  (Web.parseUrlPiece -> Right param)
  where
    PathParam param = Web.toUrlPiece param

pattern IsQueryParam :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> QueryValue
pattern IsQueryParam param <-
  QueryParam (Web.parseUrlPiece -> Right param)
  where
    IsQueryParam param = QueryParam $ Web.toUrlPiece param

pattern HasQueryFlag :: Maybe QueryValue
pattern HasQueryFlag <- Just QueryFlag

viewQuery :: Text.Text -> Query -> (Maybe QueryValue, Query)
viewQuery name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

viewQueryParam :: Web.FromHttpApiData a => Text.Text -> Query -> (Maybe a, Query)
viewQueryParam name query = case List.lookup name query of
  Just (QueryParam param) -> case Web.parseQueryParamMaybe param of
    Nothing -> (Nothing, query)
    Just value -> (Just value, List.delete (name, QueryParam param) query)
  _ -> (Nothing, query)

-- $relativeURLs
--
-- Relative URLs are useful when we want to refer to other locations within our app.
-- Thanks to bidirectional patterns, we can use the same pattern to deconstruct an incoming request
-- AND construct the relative URL that leads to itself.

data RelURL = RelURL Path Query

-- TODO: Use ToURL typeclass for Path and Query, then combine for RelURL??
renderRelURL :: RelURL -> Text.Text
renderRelURL (RelURL path query) = case (path, query) of
  ([], []) -> ""
  ([], q) -> "?" <> renderQuery q
  (p, []) -> renderPath p
  (p, q) -> renderPath p <> "?" <> renderQuery q

renderPath :: Path -> Text.Text
renderPath [] = "/"
renderPath (pathSeg : path) = "/" <> pathSeg <> loop path
  where
    loop :: Path -> Text.Text
    loop [] = ""
    loop (pathSeg : path) = "/" <> pathSeg <> loop path

renderQuery :: Query -> Text.Text
renderQuery [] = ""
renderQuery ((name, QueryFlag) : query) = name <> "&" <> renderQuery query
renderQuery ((name, QueryParam value) : query) = name <> "=" <> value <> "&" <> renderQuery query

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

    queryParam :: Atto.Parser (Text.Text, QueryValue)
    queryParam = do
      queryParamName <- Atto.takeWhile (\c -> c /= '=' && c /= '&')
      mbEquals <- Combinators.optional $ Atto.char '='
      case mbEquals of
        Nothing -> pure (queryParamName, QueryFlag)
        Just _ -> do
          queryParamValue <- Atto.takeWhile (/= '&')
          pure (queryParamName, QueryParam queryParamValue)

-- $testing
--
-- There are two ways to test in Okapi.

test ::
  Monad m =>
  OkapiT m Response ->
  Request ->
  m (Either Failure Response, State)
test okapiT request =
  (State.runStateT . Except.runExceptT . unOkapiT $ okapiT)
    (requestToState request)
  where
    requestToState :: Request -> State
    requestToState stateRequest = let stateVault = mempty in State {..}

testPure ::
  OkapiT Identity.Identity Response ->
  Request ->
  Identity.Identity (Either Failure Response, State)
testPure = test

testIO ::
  OkapiT IO Response ->
  Request ->
  IO (Either Failure Response, State)
testIO = test

-- TODO: Add common assertion helpers. Use Predicate for Contravariant interface??

assert ::
  ((Either Failure Response, State) -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assert assertion = assertion

assert200 :: (Either Failure Response, State) -> Bool
assert200 = \case
  (Right (Response 200 _ _), _) -> True
  _ -> False

assert404 :: (Either Failure Response, State) -> Bool
assert404 = \case
  (Right (Response 404 _ _), _) -> True
  _ -> False

assert500 :: (Either Failure Response, State) -> Bool
assert500 = \case
  (Right (Response 500 _ _), _) -> True
  _ -> False

testRunSession ::
  Monad m =>
  WAI.Session a ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO a
testRunSession session hoister okapiT = do
  let waiApp = app notFound hoister okapiT
  WAI.runSession session waiApp

testWithSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  WAI.Session a ->
  IO a
testWithSession hoister okapiT session = testRunSession session hoister okapiT

testRequest :: Request -> WAI.Session WAI.SResponse
testRequest = WAI.srequest . requestToSRequest
  where
    requestToSRequest :: Request -> WAI.SRequest
    requestToSRequest request@(Request mbMethod path query body headers) =
      let requestMethod = Maybe.fromMaybe HTTP.methodGet mbMethod
          sRequestBody =
            case body of
              BodyRaw lbs -> lbs
              BodyMultipart _ -> error "Must use BodyRaw for testRequest"
          rawPath = RelURL path query Function.& \relURL -> Text.encodeUtf8 $ renderRelURL relURL
          sRequestRequest = WAI.setPath (WAI.defaultRequest {WAI.requestMethod = requestMethod, WAI.requestHeaders = headers}) rawPath
       in WAI.SRequest sRequestRequest sRequestBody

-- $HasSession

type Session = Map.Map BS.ByteString BS.ByteString

class Monad m => HasSession m where
  sessionSecret :: m BS.ByteString
  getSession :: m (Maybe Session)
  putSession :: Session -> m ()

session :: (MonadOkapi m, HasSession m) => m Session
session = do
  cachedSession <- getSession
  maybe sessionInCookie pure cachedSession
  where
    sessionInCookie :: (MonadOkapi m, HasSession m) => m Session
    sessionInCookie = do
      encodedSession <- cookieCrumb "session"
      secret <- sessionSecret
      pure $ decodeSession secret encodedSession

sessionLookup :: HasSession m => MonadOkapi m => BS.ByteString -> m BS.ByteString
sessionLookup key = do
  mbValue <- Map.lookup key <$> session
  maybe next pure mbValue

sessionInsert :: HasSession m => MonadOkapi m => BS.ByteString -> BS.ByteString -> m ()
sessionInsert key value = session >>= \sesh -> putSession (Map.insert key value sesh)

sessionDelete :: HasSession m => MonadOkapi m => BS.ByteString -> m ()
sessionDelete key = session >>= \sesh -> putSession (Map.delete key sesh)

sessionClear :: HasSession m => m ()
sessionClear = putSession Map.empty

encodeSession :: BS.ByteString -> Session -> BS.ByteString
encodeSession secret session =
  let serial = HTTP.renderSimpleQuery False $ Map.toList session
      digest = HMAC.hmacGetDigest $ HMAC.hmac secret serial :: Crypto.Digest Crypto.SHA256
      b64 = BS.encodeBase64' $ Memory.convert digest
   in b64 <> serial

decodeSession :: BS.ByteString -> BS.ByteString -> Session
decodeSession secret encodedSession =
  let (b64, serial) = BS.splitAt 44 encodedSession
      mbDigest :: Maybe (Crypto.Digest Crypto.SHA256) = Crypto.digestFromByteString $ Either.fromRight BS.empty $ BS.decodeBase64 b64
   in case mbDigest of
        Nothing -> Map.empty
        Just digest ->
          if HMAC.hmacGetDigest (HMAC.hmac secret serial :: HMAC.HMAC Crypto.SHA256) == digest
            then Map.fromList $ HTTP.parseSimpleQuery serial
            else Map.empty

withSession :: (MonadOkapi m, HasSession m) => Middleware m
withSession handler = do
  mbSession <- getSession
  case mbSession of
    Nothing -> handler
    Just session -> do
      secret <- sessionSecret
      response <- handler
      response
        Function.& addSetCookie ("session", encodeSession secret session)
        Function.& pure

-- $csrfProtection

{-
class Monad m => HasCSRFProtection m where
-}
