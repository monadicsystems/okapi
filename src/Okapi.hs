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

module Okapi
  ( -- * Parser

    -- ** Types
    MonadOkapi,
    OkapiT (..),
    State,
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

    -- ** Method Parsers
    -- $methodParsers
    method,
    methodMatch,
    methodGET,
    methodPOST,
    methodHEAD,
    methodPUT,
    methodPATCH,
    methodDELETE,
    methodOPTIONS,
    methodTRACE,
    methodCONNECT,

    -- ** Path Parsers
    -- $pathParsers
    path,
    pathMatch,
    seg,
    segMatch,
    segMatchWith,

    -- ** Query Parsers
    -- $queryParsers
    query,
    queryItem,
    queryFlag,
    queryParam,
    queryList,

    -- ** Body Parsers
    -- $bodyParsers
    body,
    bodyJSON,
    bodyForm,

    -- ** Header Parsers
    -- $headerParsers
    headers,
    header,
    cookie,
    crumb,
    basicAuth,

    -- * Error

    -- ** Types
    Failure (..),

    -- ** Helpers
    -- $errorHelpers
    next,
    throw,
    (<!>),
    guardThrow,

    -- * Response
    -- $response

    -- ** Types
    Response (..),
    Status,
    ResponseBody (..),

    -- ** Values
    ok,
    notFound,
    redirect,

    -- ** Setters
    setStatus,
    setHeaders,
    setHeader,
    setBody,
    setBodyRaw,
    setBodyFile,
    setBodyEventSource,
    setPlaintext,
    setJSON,
    setHTML,

    -- *
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
import qualified Control.Monad.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor as Bifunctor
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
import qualified Network.Wai.Handler.Warp as WAI
import qualified Network.Wai.Handler.WarpTLS as WAI
import qualified Network.Wai.Handler.WebSockets as WAI
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.Wai.Internal as WAI
import qualified Network.Wai.Middleware.Gzip as WAI
import qualified Network.Wai.Test as WAI
import qualified Network.WebSockets as WebSockets
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

-- $parsing
--
-- At it's core, Okapi is an HTTP parser.

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

instance Morph.MonadTrans OkapiT where
  lift :: Monad m => m a -> OkapiT m a
  lift action = OkapiT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor OkapiT where
  hoist :: Monad m => (forall a. m a -> n a) -> OkapiT m b -> OkapiT n b
  hoist nat okapiT = OkapiT . Except.ExceptT . State.StateT $ (nat . State.runStateT (Except.runExceptT $ unOkapiT okapiT))

-- TODO: Implement for more interesting monad comprehensions
-- instance Zip.MonadZip OkapiT where
--   mzip :: OkapiT m a -> OkapiT m b -> OkapiT m (a, b)
--   mzip (OkapiT (Except.ExceptT (State.StateT mx))) (OkapiT (Except.ExceptT (State.StateT my))) = OkapiT . Except.ExceptT . State.StateT $ \s -> do
--     (eitherX, stateX) <- mx s
--     (eitherY, stateY) <- my s
--     case (eitherX, eitherY) of
--       Left Skip -> pure (Left Skip, s)
--       Left error@(Error _) -> pure (Left error, s)
--       Right y -> pure (Right y, stateY)
--       Left error@(Error _) -> pure (Left error, s)
--       Right x -> pure (Right x, stateX)

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

type Method = Maybe HTTP.Method

type Path = [Text.Text]

type Query = [QueryItem]

type QueryItem = (Text.Text, QueryValue)

data QueryValue = QueryParam Text.Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

type Body = LBS.ByteString

type Headers = [Header]

type Header = (HeaderName, BS.ByteString)

type HeaderName = HTTP.HeaderName

type Cookie = [Crumb]

type Crumb = (Text.Text, Text.Text)

-- $parsers
--
-- These are the parsers that you'll use to build you own app.

-- | Parses without modifying the state, even if it succeeds.
look :: MonadOkapi m => m a -> m a
look parser = do
  state <- State.get
  result <- parser
  State.put state
  return result

-- | Parses the entire request.
request :: MonadOkapi m => m Request
request = Request <$> (Just <$> method) <*> path <*> query <*> body <*> headers

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request method.

method :: MonadOkapi m => m HTTP.Method
method = do
  maybeMethod <- State.gets (requestMethod . stateRequest)
  case maybeMethod of
    Nothing -> next
    Just method' -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestMethod = Nothing}})
      pure method'

-- |
-- >>> let parser = method "CUSTOM" >> respond ok
-- >>> result <- testParserIO parser (TestRequest "CUSTOM" [] "" "")
-- >>> assertResponse is200 result
-- True
methodMatch :: forall m. MonadOkapi m => HTTP.Method -> m ()
methodMatch desiredMethod = do
  currentMethod <- method
  if desiredMethod == currentMethod
    then pure ()
    else next

-- |
-- >>> let parser = get >> respond ok
-- >>> result <- testParserIO parser $ request GET "" "" []
-- >>> assertResponse is200 result
-- True
methodGET :: forall m. MonadOkapi m => m ()
methodGET = methodMatch HTTP.methodGet

-- |
-- >>> let parser = post >> respond ok
-- >>> result <- testParserIO parser (TestRequest "POST" [] "" "")
-- >>> assertResponse is200 result
-- True
methodPOST :: forall m. MonadOkapi m => m ()
methodPOST = methodMatch HTTP.methodPost

-- |
-- >>> let parser = Okapi.Parser.head >> respond ok
-- >>> result <- testParserIO parser (TestRequest "HEAD" [] "" "")
-- >>> assertResponse is200 result
-- True
methodHEAD :: forall m. MonadOkapi m => m ()
methodHEAD = methodMatch HTTP.methodHead

-- |
-- >>> let parser = put >> respond ok
-- >>> result <- testParserIO parser (TestRequest "PUT" [] "" "")
-- >>> assertResponse is200 result
-- True
methodPUT :: forall m. MonadOkapi m => m ()
methodPUT = methodMatch HTTP.methodPut

-- |
-- >>> let parser = delete >> respond ok
-- >>> result <- testParserIO parser (TestRequest "DELETE" [] "" "")
-- >>> assertResponse is200 result
-- True
methodDELETE :: forall m. MonadOkapi m => m ()
methodDELETE = methodMatch HTTP.methodDelete

-- |
-- >>> let parser = trace >> respond ok
-- >>> result <- testParserIO parser (TestRequest "TRACE" [] "" "")
-- >>> assertResponse is200 result
-- True
methodTRACE :: forall m. MonadOkapi m => m ()
methodTRACE = methodMatch HTTP.methodTrace

-- |
-- >>> let parser = connect >> respond ok
-- >>> result <- testParserIO parser (TestRequest "CONNECT" [] "" "")
-- >>> assertResponse is200 result
-- True
methodCONNECT :: forall m. MonadOkapi m => m ()
methodCONNECT = methodMatch HTTP.methodConnect

-- |
-- >>> let parser = options >> respond ok
-- >>> result <- testParserIO parser (TestRequest "OPTIONS" [] "" "")
-- >>> assertResponse is200 result
-- True
methodOPTIONS :: forall m. MonadOkapi m => m ()
methodOPTIONS = methodMatch HTTP.methodOptions

-- |
-- >>> let parser = patch >> respond ok
-- >>> result <- testParserIO parser (TestRequest "PATCH" [] "" "")
-- >>> assertResponse is200 result
-- True
methodPATCH :: forall m. MonadOkapi m => m ()
methodPATCH = methodMatch HTTP.methodPatch

-- $pathParsers
--
-- These are the path parsers.

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
path :: MonadOkapi m => m [Text.Text]
path = Combinators.many seg

pathMatch :: MonadOkapi m => Path -> m ()
pathMatch desiredPath = do
  currentPath <- path
  if currentPath == desiredPath
    then pure ()
    else next

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
seg :: Web.FromHttpApiData a => MonadOkapi m => m a
seg = do
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

segMatch :: forall a m. (Eq a, Web.FromHttpApiData a, MonadOkapi m) => a -> m ()
segMatch desiredParam = segMatchWith (desiredParam ==)

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
segMatchWith :: forall a m. (Web.FromHttpApiData a, MonadOkapi m) => (a -> Bool) -> m ()
segMatchWith predicate = do
  param <- seg
  if predicate param
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

queryItem :: MonadOkapi m => Text.Text -> m QueryItem
queryItem queryItemName = do
  maybeQueryItem <- State.gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName') . requestQuery . stateRequest)
  case maybeQueryItem of
    Nothing -> next
    Just queryItem -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = List.delete queryItem $ requestQuery $ stateRequest state}})
      pure queryItem

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
  (_, queryItemValue) <- queryItem queryItemName
  case queryItemValue of
    QueryFlag -> next
    QueryParam valueText -> maybe next pure (Web.parseQueryParamMaybe valueText)

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
queryFlag :: forall a m. MonadOkapi m => Text.Text -> m ()
queryFlag queryItemName = do
  (_, queryItemValue) <- queryItem queryItemName
  case queryItemValue of
    QueryFlag -> pure ()
    _ -> next

queryList :: (Web.FromHttpApiData a, MonadOkapi m) => Text.Text -> m [a]
queryList = undefined

-- $bodyParsers

-- | TODO: Parse body in chunks abstraction?
body :: forall m. MonadOkapi m => m Body
body = do
  currentBody <- State.gets (requestBody . stateRequest)
  if LBS.null currentBody
    then next
    else do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestBody = ""}})
      pure currentBody

bodyJSON :: forall a m. (MonadOkapi m, Aeson.FromJSON a) => m a
bodyJSON = do
  lbs <- body
  maybe next pure (Aeson.decode lbs)

bodyForm :: forall a m. (MonadOkapi m, Web.FromForm a) => m a
bodyForm = do
  lbs <- body
  maybe next pure (eitherToMaybe $ Web.urlDecodeAsForm lbs)
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- TODO: Add abstraction for multipart forms

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

cookie :: forall m. MonadOkapi m => m Cookie
cookie = do
  cookieValue <- header "Cookie"
  pure $ Web.parseCookiesText cookieValue

crumb :: forall m. MonadOkapi m => Text.Text -> m Crumb
crumb name = do
  cookieValue <- cookie
  case List.lookup name cookieValue of
    Nothing -> next
    Just crumbValue -> do
      let crumb = (name, crumbValue)
      -- TODO: Needs testing to see if state is restored properly
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookiesText $ List.delete crumb cookieValue) : requestHeaders (stateRequest state)}})
      pure crumb

basicAuth :: forall m. MonadOkapi m => m (Text.Text, Text.Text)
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

-- $ Completion Checks

methodEnd :: MonadOkapi m => m ()
methodEnd = do
  maybeMethod <- Combinators.optional method
  case maybeMethod of
    Nothing -> pure ()
    Just _ -> next

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
pathEnd :: MonadOkapi m => m ()
pathEnd = do
  currentPath <- path
  if List.null currentPath
    then pure ()
    else next

queryEnd :: MonadOkapi m => m ()
queryEnd = do
  currentQuery <- query
  if List.null currentQuery
    then pure ()
    else next

headersEnd :: MonadOkapi m => m ()
headersEnd = do
  currentHeaders <- headers
  if List.null currentHeaders
    then pure ()
    else next

cookieEnd :: MonadOkapi m => m ()
cookieEnd = do
  currentCookie <- cookie
  if List.null currentCookie
    then pure ()
    else next

bodyEnd :: MonadOkapi m => m ()
bodyEnd = do
  currentBody <- body
  if LBS.null currentBody
    then pure ()
    else next

requestEnd :: MonadOkapi m => m ()
requestEnd = do
  methodEnd
  pathEnd
  queryEnd
  headersEnd
  bodyEnd

-- $error

-- | Represents the two variants of failure that can occur when parsing a HTTP request.
data Failure = Skip | Error Response

instance Show Failure where
  show Skip = "Skipped"
  show (Error _) = "Error returned"

next :: forall a m. MonadOkapi m => m a
next = Except.throwError Skip

throw :: forall a m. MonadOkapi m => Response -> m a
throw = Except.throwError . Error

(<!>) :: forall a m. MonadOkapi m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: forall a m. MonadOkapi m => Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

-- $response

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

setJSON :: forall a. Aeson.ToJSON a => a -> Response -> Response
setJSON value response =
  response
    Function.& setHeader ("Content-Type", "application/json")
    Function.& setBodyRaw (Aeson.encode value)

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

-- $execution

run ::
  Monad m =>
  Int ->
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  IO ()
run port hoister defaultResponse okapiT = WAI.run port $ app hoister defaultResponse okapiT

runTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  IO ()
runTLS tlsSettings settings hoister defaultResponse okapiT = WAI.runTLS tlsSettings settings $ app hoister defaultResponse okapiT

runWebsockets ::
  Monad m =>
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  Int ->
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  IO ()
runWebsockets connSettings serverApp port hoister defaultResponse okapiT = WAI.run port $ websocketsApp connSettings serverApp hoister defaultResponse okapiT

runWebsocketsTLS ::
  Monad m =>
  WAI.TLSSettings ->
  WAI.Settings ->
  WebSockets.ConnectionOptions ->
  WebSockets.ServerApp ->
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  IO ()
runWebsocketsTLS tlsSettings settings connSettings serverApp hoister defaultResponse okapiT = WAI.runTLS tlsSettings settings $ websocketsApp connSettings serverApp hoister defaultResponse okapiT

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
      requestBody <- WAI.strictRequestBody waiRequest -- TODO: Use lazy request body???
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
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  WAI.Application
websocketsApp connSettings serverApp hoister defaultResponse okapiT =
  let backupApp = app hoister defaultResponse okapiT
   in WebSockets.websocketsOr connSettings serverApp backupApp

-- $middleware

-- | A middleware takes an action that returns a @Response@ and can modify the action in various ways
type Middleware m = m Response -> m Response

applyMiddlewares :: MonadOkapi m => [Middleware m] -> Middleware m
applyMiddlewares ms handler =
  Prelude.foldl (\handler m -> m handler) handler ms

clearHeadersMiddleware :: MonadOkapi m => Middleware m
clearHeadersMiddleware handler = setHeaders [] <$> handler

prefixPathMiddleware :: MonadOkapi m => [Text.Text] -> Middleware m
prefixPathMiddleware prefix handler = pathMatch prefix >> handler

-- | TODO: Is this needed? Idea taken from OCaml Dream framework
scope :: MonadOkapi m => [Text.Text] -> [Middleware m] -> Middleware m
scope prefix middlewares handler = pathMatch prefix >> applyMiddlewares middlewares handler

-- $ Bidirectional Route Patterns

type Router m = Path -> m Response

route :: MonadOkapi m => Router m -> m Response
route router = path >>= router

-- $patterns

-- $methodPatterns

pattern GET :: Method
pattern GET = Just "GET"

pattern POST :: Method
pattern POST = Just "POST"

pattern DELETE :: Method
pattern DELETE = Just "DELETE"

-- $pathPatterns

pattern Seg :: (Web.ToHttpApiData a, Web.FromHttpApiData a) => a -> Text.Text
pattern Seg param <-
  (Web.parseUrlPiece -> Right param)
  where
    Seg param = Web.toUrlPiece param

-- $queryPatterns

pattern HasQueryParam :: Web.FromHttpApiData a => a -> Maybe QueryValue
pattern HasQueryParam value <- Just (QueryParam (Web.parseQueryParam -> Right value))

pattern HasQueryFlag :: Maybe QueryValue
pattern HasQueryFlag <- Just QueryFlag

queryView :: Text.Text -> Query -> (Maybe QueryValue, Query)
queryView name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

-- $relativeURLs

data RelURL = RelURL Path Query

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

-- TODO: Use ToURL typeclass for Path and Query, then combine for RelURL??
renderRelURL :: RelURL -> Text.Text
renderRelURL (RelURL path query) = case (path, query) of
  ([], []) -> ""
  ([], q) -> "?" <> queryToURL q
  (p, []) -> pathToURL p
  (p, q) -> pathToURL p <> "?" <> queryToURL q
  where
    queryToURL :: Query -> Text.Text
    queryToURL [] = ""
    queryToURL ((name, QueryFlag) : query) = name <> "&" <> queryToURL query
    queryToURL ((name, QueryParam value) : query) = name <> "=" <> value <> "&" <> queryToURL query

    pathToURL :: Path -> Text.Text
    pathToURL [] = ""
    pathToURL (pathSeg : path) = "/" <> pathSeg <> pathToURL path

-- $ testing

testParser ::
  Monad m =>
  OkapiT m Response ->
  Request ->
  m (Either Failure Response, State)
testParser okapiT request =
  (State.runStateT . Except.runExceptT . unOkapiT $ okapiT)
    (requestToState request)
  where
    requestToState :: Request -> State
    requestToState stateRequest = let stateVault = mempty in State {..}

testParserIO ::
  OkapiT IO Response ->
  Request ->
  IO (Either Failure Response, State)
testParserIO = testParser

-- ASSERTION FUNCTIONS TODO: Add common assertion helpers

assert ::
  ((Either Failure Response, State) -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assert assertion = assertion

assertNext = undefined

assert200Response = undefined

assert404Error = undefined

-- $testingWithWAITest

runSession ::
  Monad m =>
  WAI.Session a ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO a
runSession session hoister okapiT = do
  let waiApp = app hoister notFound okapiT
  WAI.runSession session waiApp

withSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  WAI.Session a ->
  IO a
withSession hoister okapiT session = runSession session hoister okapiT

testRequest :: Request -> WAI.Session WAI.SResponse
testRequest = WAI.srequest . requestToSRequest
  where
    requestToSRequest :: Request -> WAI.SRequest
    requestToSRequest request@(Request mbMethod path query body headers) =
      let requestMethod = Maybe.fromMaybe HTTP.methodGet mbMethod
          sRequestBody = body
          rawPath = RelURL path query Function.& \relURL -> Text.encodeUtf8 $ renderRelURL relURL
          sRequestRequest = WAI.setPath (WAI.defaultRequest {WAI.requestMethod = requestMethod, WAI.requestHeaders = headers}) rawPath
       in WAI.SRequest sRequestRequest sRequestBody
