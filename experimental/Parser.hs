{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | This module exports parsers for verifying the existance of and extracting data from incoming HTTP requests.
module Okapi.Parser
  ( -- * Method Parsers

    -- | Parsers for matching the Method of a request
    get,
    post,
    Okapi.Parser.head,
    put,
    delete,
    trace,
    connect,
    options,
    patch,
    anyMethod,
    method,

    -- * Path parsers

    -- | Parsers for matching or extracting data from the path of a request
    pathSeg,
    path,
    pathParam,
    pathParamRaw,
    pathSegWith,
    pathWildcard,

    -- * Query Parsers

    -- | Parsers for matching of extracting data from the query parameters of a request
    queryParam,
    queryParamRaw,
    queryFlag,

    -- * Header Parsers

    -- | Parsers for extracting data from the headers of a request.
    -- Useful for authentication, cookies, and other request metadata, like the Client's preferred media type(s)
    basicAuth,
    cookies,
    header,

    -- * Body Parsers

    -- | Parsers for extracting data from the body of a request
    bodyJSON,
    bodyURLEncoded,
    bodyRaw,

    -- * Response Helpers
    respond,

    -- * Error Helpers

    -- | Various helper functions for throwing parser errors
    -- See for more information on how throwing error works
    next,
    throw,
    (<!>),
    guardThrow,
    optionalThrow,
    optionThrow,

    -- * State Checkers

    -- | Functions for checking the state of a parser.
    -- Useful for checking if certain parts of the request have been completely parsed before continuing with another action.
    -- See the implementation of @respond@ for an example of how these functions can be used.
    methodParsed,
    pathParsed,
    queryParsed,
    headersParsed,
    bodyParsed,

    -- * Internal Parsers
    parseMethod,
    parsePathSeg,
    parseQueryItem,
    parsePath,
    Okapi.Parser.parseQuery,
    parseHeaders,
    parseBody,
    parseRequest,

    -- * Special
    match,
  )
where

import Control.Monad.Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Base64
import Network.HTTP.Types (parseQuery)
import qualified Network.HTTP.Types as HTTP
import Okapi.Types
import qualified Web.Cookie as Cookie
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (head)

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Okapi.Effect.Response
-- >>> import Okapi.Test

-- |
-- >>> let parser = get >> respond ok
-- >>> result <- testIO parser $ request GET "" "" []
-- >>> assertResponse is200 result
-- True
get :: forall m. MonadHTTP m => m ()
get = method HTTP.methodGet

-- |
-- >>> let parser = post >> respond ok
-- >>> result <- testIO parser (TestRequest "POST" [] "" "")
-- >>> assertResponse is200 result
-- True
post :: forall m. MonadHTTP m => m ()
post = method HTTP.methodPost

-- |
-- >>> let parser = Okapi.Parser.head >> respond ok
-- >>> result <- testIO parser (TestRequest "HEAD" [] "" "")
-- >>> assertResponse is200 result
-- True
head :: forall m. MonadHTTP m => m ()
head = method HTTP.methodHead

-- |
-- >>> let parser = put >> respond ok
-- >>> result <- testIO parser (TestRequest "PUT" [] "" "")
-- >>> assertResponse is200 result
-- True
put :: forall m. MonadHTTP m => m ()
put = method HTTP.methodPut

-- |
-- >>> let parser = delete >> respond ok
-- >>> result <- testIO parser (TestRequest "DELETE" [] "" "")
-- >>> assertResponse is200 result
-- True
delete :: forall m. MonadHTTP m => m ()
delete = method HTTP.methodDelete

-- |
-- >>> let parser = trace >> respond ok
-- >>> result <- testIO parser (TestRequest "TRACE" [] "" "")
-- >>> assertResponse is200 result
-- True
trace :: forall m. MonadHTTP m => m ()
trace = method HTTP.methodTrace

-- |
-- >>> let parser = connect >> respond ok
-- >>> result <- testIO parser (TestRequest "CONNECT" [] "" "")
-- >>> assertResponse is200 result
-- True
connect :: forall m. MonadHTTP m => m ()
connect = method HTTP.methodConnect

-- |
-- >>> let parser = options >> respond ok
-- >>> result <- testIO parser (TestRequest "OPTIONS" [] "" "")
-- >>> assertResponse is200 result
-- True
options :: forall m. MonadHTTP m => m ()
options = method HTTP.methodOptions

-- |
-- >>> let parser = patch >> respond ok
-- >>> result <- testIO parser (TestRequest "PATCH" [] "" "")
-- >>> assertResponse is200 result
-- True
patch :: forall m. MonadHTTP m => m ()
patch = method HTTP.methodPatch

-- |
-- >>> let parser = anyMethod >> respond ok
-- >>> result <- testIO parser (TestRequest "FOOBLAH" [] "" "")
-- >>> assertResponse is200 result
-- True
anyMethod :: forall m. MonadHTTP m => m ()
anyMethod = parseMethod >> pure ()

-- |
-- >>> let parser = method "CUSTOM" >> respond ok
-- >>> result <- testIO parser (TestRequest "CUSTOM" [] "" "")
-- >>> assertResponse is200 result
-- True
method :: forall m. MonadHTTP m => HTTP.Method -> m ()
method method = do
  method' <- parseMethod
  if method == method'
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
-- >>> result <- testIO parser (TestRequest "GET" [] "/store/clothing" "")
-- >>> assertResponse is200 result
-- True
pathSeg :: forall m. MonadHTTP m => Text.Text -> m ()
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
-- >>> result <- testIO parser (TestRequest "GET" [] "/store/clothing" "")
-- >>> assertResponse is200 result
-- True
path :: forall m. MonadHTTP m => [Text.Text] -> m ()
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
-- >>> result <- testIO parser (TestRequest "GET" [] "/product/242301" "")
-- >>> assertResponse is200 result
-- True
pathParam :: forall a m. (MonadHTTP m, Web.FromHttpApiData a) => m a
pathParam = do
  pathSeg <- parsePathSeg
  maybe next pure (Web.parseUrlPieceMaybe pathSeg)

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
-- >>> result <- testIO parser (TestRequest "GET" [] "/product/242301" "")
-- >>> assertResponse is200 result
-- True
pathParamRaw :: forall m. MonadHTTP m => m Text.Text
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
-- >>> result1 <- testIO parser (TestRequest "GET" [] "/product/242301" "")
-- >>> assertResponse is200 result1
-- True
-- >>> result2 <- testIO parser (TestRequest "GET" [] "/product/5641" "")
-- >>> assertFailure isSkip result2
-- True
pathSegWith :: forall m. MonadHTTP m => (Text.Text -> Bool) -> m ()
pathSegWith predicate = do
  pathSeg <- parsePathSeg
  if predicate pathSeg
    then pure ()
    else next

-- | Parses all the remaining path segments of a request
pathWildcard :: forall m. MonadHTTP m => m (NonEmpty.NonEmpty Text.Text)
pathWildcard = do
  segs <- some pathParamRaw
  case segs of
    [] -> next
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
-- >>> result <- testIO parser (TestRequest "GET" [] "/product?min_qty=2" "")
-- >>> assertResponse is200 result
-- True
-- >>> assertResponse (hasBodyRaw "5") result
-- True
queryParam :: forall a m. (MonadHTTP m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  case queryItemValue of
    QueryFlag -> next
    QueryParam valueText -> maybe next pure (Web.parseQueryParamMaybe valueText)

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
-- >>> result <- testIO parser (TestRequest "GET" [] "/flip/my/bit?value=b0" "")
-- >>> assertResponse (hasBodyRaw "1") result
-- True
queryParamRaw :: forall m. MonadHTTP m => Text.Text -> m Text.Text
queryParamRaw queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  case queryItemValue of
    QueryFlag -> next
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
-- >>> result1 <- testIO parser (TestRequest "GET" [] "/users?admin" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\"]") result1
-- True
-- >>> result2 <- testIO parser (TestRequest "GET" [] "/users?admin=foobarbaz" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\"]") result2
-- True
-- >>> result3 <- testIO parser (TestRequest "GET" [] "/users" "")
-- >>> assertResponse (hasBodyRaw "[\"Derek\",\"Alice\",\"Bob\",\"Casey\",\"Alex\",\"Larry\"]") result3
-- True
queryFlag :: forall a m. MonadHTTP m => Text.Text -> m Bool
queryFlag queryItemName = do
  maybeQueryItem <- optional $ parseQueryItem queryItemName
  pure $ case maybeQueryItem of
    Nothing -> False
    Just _ -> True

queryParams :: forall m. MonadHTTP m => m (Map Text.Text Text.Text)
queryParams = undefined

-- HEADER HELPERS

basicAuth :: forall m. MonadHTTP m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case decodeBase64 encodedCreds of
        Left _ -> next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> next
    _ -> next

-- TODO: cookie :: forall m. MonadHTTP m => Cookie

cookies :: forall m. MonadHTTP m => m Cookies
cookies = do
  cookiesValue <- header "Cookie"
  pure $ Cookie.parseCookiesText cookiesValue

-- TODO: Any checks required??
header :: forall m. MonadHTTP m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  (_headerName, headerValue) <- parseHeader headerName
  pure headerValue

-- TODO: headers :: forall m. MonadHTTP m => m Headers

-- BODY HELPERS

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyJSON :: forall a m. (MonadHTTP m, Aeson.FromJSON a) => m a
bodyJSON = do
  body <- bodyRaw
  maybe next pure (Aeson.decode body)

bodyURLEncoded :: forall a m. (MonadHTTP m, Web.FromForm a) => m a
bodyURLEncoded = do
  body <- bodyRaw
  maybe next pure (eitherToMaybe $ Web.urlDecodeAsForm body)
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- TODO: bodyFile functions for file uploads to server?
bodyRaw :: forall m. MonadHTTP m => m LBS.ByteString
bodyRaw = parseBody

-- Response helpers

respond :: forall m. MonadHTTP m => Response -> m Response
respond response = do
  check1 <- methodParsed
  check2 <- pathParsed
  -- check3 <- queryParsed
  if check1 && check2 then return response else next

-- TODO: add end parser similar to <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>

-- Error HELPERS

next :: forall a m. MonadHTTP m => m a
next = Except.throwError Skip

throw :: forall a m. MonadHTTP m => Response -> m a
throw = Except.throwError . Error

(<!>) :: forall a m. MonadHTTP m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: forall a m. MonadHTTP m => Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

optionalThrow :: forall a m. MonadHTTP m => m a -> m (Maybe a)
optionalThrow parser = (Just <$> parser) <!> pure Nothing

optionThrow :: forall a m. MonadHTTP m => a -> m a -> m a
optionThrow value parser = do
  mbValue <- optionalThrow parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'

-- State Checks

methodParsed :: MonadHTTP m => m Bool
methodParsed = State.gets stateRequestMethodParsed

pathParsed :: MonadHTTP m => m Bool
pathParsed = State.gets (Prelude.null . requestPath . stateRequest)

queryParsed :: MonadHTTP m => m Bool
queryParsed = State.gets (Prelude.null . requestQuery . stateRequest)

headersParsed :: MonadHTTP m => m Bool
headersParsed = State.gets (Prelude.null . requestHeaders . stateRequest)

bodyParsed :: MonadHTTP m => m Bool
bodyParsed = State.gets stateRequestBodyParsed

-- PRIMITIVE PARSERS (BELOW IS INTERNAL)

parseMethod :: MonadHTTP m => m HTTP.Method
parseMethod = do
  isMethodParsed <- methodParsed
  if isMethodParsed
    then next
    else do
      method <- State.gets (requestMethod . stateRequest)
      State.modify (\state -> state {stateRequestMethodParsed = True})
      pure method

parsePath :: MonadHTTP m => m [Text.Text]
parsePath = some parsePathSeg

parsePathSeg :: MonadHTTP m => m Text.Text
parsePathSeg = do
  maybePathSeg <- State.gets (safeHead . requestPath . stateRequest)
  case maybePathSeg of
    Nothing -> next
    Just pathSeg -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestPath = Prelude.drop 1 $ requestPath $ stateRequest state}})
      pure pathSeg
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

parseQueryItem :: MonadHTTP m => Text.Text -> m QueryItem
parseQueryItem queryItemName = do
  maybeQueryItem <- State.gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName') . requestQuery . stateRequest)
  case maybeQueryItem of
    Nothing -> next
    Just queryItem -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = List.delete queryItem $ requestQuery $ stateRequest state}})
      pure queryItem

parseQuery :: MonadHTTP m => m Query
parseQuery = do
  query <- State.gets (requestQuery . stateRequest)
  State.modify (\state -> state {stateRequest = (stateRequest state) {requestQuery = []}})
  pure query

parseHeaders :: MonadHTTP m => m Headers
parseHeaders = do
  headers <- State.gets (requestHeaders . stateRequest)
  State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = []}})
  pure headers

parseHeader :: MonadHTTP m => HTTP.HeaderName -> m Header
parseHeader headerName = do
  maybeHeader <- State.gets (Foldable.find (\(headerName', _) -> headerName == headerName') . requestHeaders . stateRequest)
  case maybeHeader of
    Nothing -> next
    Just header -> do
      State.modify (\state -> state {stateRequest = (stateRequest state) {requestHeaders = List.delete header $ requestHeaders $ stateRequest state}})
      pure header

parseBody :: forall m. MonadHTTP m => m LBS.ByteString
parseBody = do
  isBodyParsed <- bodyParsed
  if isBodyParsed
    then next
    else do
      body <- State.gets (requestBody . stateRequest)
      State.modify (\state -> state {stateRequestBodyParsed = True})
      pure body

parseRequest :: MonadHTTP m => m Request
parseRequest = Request <$> parseMethod <*> parsePath <*> Okapi.Parser.parseQuery <*> parseBody <*> parseHeaders

match :: MonadHTTP m => (Request -> m Response) -> m Response
match matcher = parseRequest >>= matcher

-- TODO: Probably don't need??? I don't think so after some thought
{-
routeToFile :: MonadHTTP m => Status -> Headers -> m Response
routeToFile status headers = do
  nonEmptyPath <- pathWildcard
  let filePath = nonEmptyPathToFilePath nonEmptyPath
  respond $ Response status headers $ ResponseBodyFile filePath
  where
    nonEmptyPathToFilePath :: NonEmpty Text -> FilePath
    nonEmptyPathToFilePath (base :| path) = unpack $ base <> Text.intercalate "/" path

static :: MonadHTTP m => [Text] ->
static
-}

lookupQuery :: MonadHTTP m => Text -> Query -> m QueryValue
lookupQuery name query = maybe next pure (List.lookup name query)

lookupHeaders :: MonadHTTP m => HeaderName -> Headers -> m BS.ByteString
lookupHeaders name headers = maybe next pure (List.lookup name headers)

lookupForm :: (MonadHTTP m, Web.FromHttpApiData a) => Text -> Body -> m a
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
