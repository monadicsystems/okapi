{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Request where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Parse as WAI
import qualified Okapi.Server.Failure as Failure
import qualified Okapi.Server.Request as Request
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

class (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Request.StateM m) => RequestM m

-- | Parses the entire request.
request :: RequestM m => m Request.Request
request = Request.Request <$> method <*> path <*> query <*> body <*> headers <*> vault

requestEnd :: RequestM m => m ()
requestEnd = do
  methodEnd
  pathEnd
  queryEnd
  headersEnd
  bodyEnd

-- $ methodParsers
--
-- These are parsers for parsing the HTTP request method.

method :: RequestM m => m Request.Method
method = do
  maybeMethod <- Request.gets Request.method
  case maybeMethod of
    Nothing -> pure Nothing
    method'@(Just _) -> do
      Request.modify (\request -> request {Request.method = Nothing})
      pure method'

methodGET :: RequestM m => m ()
methodGET = Failure.is method $ Just HTTP.methodGet

methodPOST :: RequestM m => m ()
methodPOST = Failure.is method $ Just HTTP.methodPost

methodHEAD :: RequestM m => m ()
methodHEAD = Failure.is method $ Just HTTP.methodHead

methodPUT :: RequestM m => m ()
methodPUT = Failure.is method $ Just HTTP.methodPut

methodDELETE :: RequestM m => m ()
methodDELETE = Failure.is method $ Just HTTP.methodDelete

methodTRACE :: RequestM m => m ()
methodTRACE = Failure.is method $ Just HTTP.methodTrace

methodCONNECT :: RequestM m => m ()
methodCONNECT = Failure.is method $ Just HTTP.methodConnect

methodOPTIONS :: RequestM m => m ()
methodOPTIONS = Failure.is method $ Just HTTP.methodOptions

methodPATCH :: RequestM m => m ()
methodPATCH = Failure.is method $ Just HTTP.methodPatch

methodEnd :: RequestM m => m ()
methodEnd = do
  maybeMethod <- Combinators.optional method
  case maybeMethod of
    Nothing -> pure ()
    Just _ -> Failure.next

-- $pathParsers
--
-- These are the path parsers.

-- | Parses and discards mutiple path segments matching the values and order of the given @[Text]@ value
path :: RequestM m => m [Text.Text]
path = Combinators.many pathParam

-- | Parses and discards a single path segment matching the given @Text@ value
pathParam :: (Web.FromHttpApiData a, RequestM m) => m a
pathParam = do
  maybePathSeg <- Request.gets (safeHead . Request.path)
  case maybePathSeg of
    Nothing -> Failure.next
    Just pathSeg -> do
      Request.modify (\request -> request {Request.path = Prelude.drop 1 $ Request.path request})
      maybe Failure.next pure (Web.parseUrlPieceMaybe pathSeg)
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
pathEnd :: RequestM m => m ()
pathEnd = do
  currentPath <- path
  if List.null currentPath
    then pure ()
    else Failure.next

-- $queryParsers
--
-- These are the query parsers.

query :: RequestM m => m Request.Query
query = do
  query <- Request.gets Request.query
  Request.modify (\request -> request {Request.query = []})
  pure query

queryValue :: RequestM m => Text.Text -> m Request.QueryValue
queryValue queryItemName = do
  maybeQueryItem <- Request.gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName') . Request.query)
  case maybeQueryItem of
    Nothing -> Failure.next
    Just queryItem@(_, queryValue) -> do
      Request.modify (\request -> request {Request.query = List.delete queryItem $ Request.query request})
      pure queryValue

-- | Parses the value of a query parameter with the given type and name
queryParam :: (Web.FromHttpApiData a, RequestM m) => Text.Text -> m a
queryParam queryItemName = do
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    Request.QueryFlag -> Failure.next
    Request.QueryParam valueText -> maybe Failure.next pure (Web.parseQueryParamMaybe valueText)

-- | Test for the existance of a query flag
queryFlag :: RequestM m => Text.Text -> m ()
queryFlag queryItemName = do
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    Request.QueryFlag -> pure ()
    _ -> Failure.next

queryParamList :: (Web.FromHttpApiData a, RequestM m) => Text.Text -> m (NonEmpty.NonEmpty a)
queryParamList = Combinators.NonEmpty.some . queryParam

queryEnd :: RequestM m => m ()
queryEnd = do
  currentQuery <- query
  if List.null currentQuery
    then pure ()
    else Failure.next

-- $bodyParsers

-- | For getting the raw body of the request.
body :: RequestM m => m Request.Body
body = do
  currentBody <- Request.gets Request.body
  case currentBody of
    Request.Raw (LBS.null -> True) -> pure currentBody
    Request.Multipart ([], []) -> pure currentBody
    Request.Raw _ -> do
      Request.modify (\request -> request {Request.body = Request.Raw ""})
      pure currentBody
    Request.Multipart _ -> do
      Request.modify (\request -> request {Request.body = Request.Multipart ([], [])})
      pure currentBody

-- | Parse request body as JSON
bodyJSON :: (Aeson.FromJSON a, RequestM m) => m a
bodyJSON = do
  body' <- body
  case body' of
    Request.Raw lbs -> maybe Failure.next pure (Aeson.decode lbs)
    Request.Multipart _ -> Failure.next

-- | Parse URLEncoded form parameters from request body
bodyURLEncoded :: (Web.FromForm a, RequestM m) => m a
bodyURLEncoded = do
  body' <- body
  case body' of
    Request.Raw lbs -> maybe Failure.next pure (eitherToMaybe $ Web.urlDecodeAsForm lbs)
    Request.Multipart _ -> Failure.next
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- | Parse multipart form data from request body
bodyMultipart :: RequestM m => m ([WAI.Param], [WAI.File LBS.ByteString])
bodyMultipart = do
  body' <- body
  case body' of
    Request.Raw _ -> Failure.next
    Request.Multipart formData -> pure formData

-- | Parse a single form parameter
formParam :: forall a m. (Web.FromHttpApiData a, RequestM m) => BS.ByteString -> m a
formParam paramName = do
  body' <- body
  case body' of
    Request.Raw lbs -> do
      case Web.urlDecodeParams lbs of
        Left _ -> Failure.next
        Right params ->
          case lookup (Text.decodeUtf8 paramName) params of
            Nothing -> Failure.next
            Just paramValue -> do
              paramValue' <- maybe Failure.next pure (Web.parseQueryParamMaybe paramValue)
              let newParams = List.delete (Text.decodeUtf8 paramName, paramValue) params
              Request.modify (\request -> request {Request.body = Request.Raw $ Web.urlEncodeParams newParams})
              pure paramValue'
    Request.Multipart (params, files) -> do
      case lookup paramName params of
        Nothing -> Failure.next
        Just paramValue -> do
          paramValue' <- maybe Failure.next pure (Web.parseQueryParamMaybe $ Text.decodeUtf8 paramValue)
          let newParams = List.delete (paramName, paramValue) params
          Request.modify (\request -> request {Request.body = Request.Multipart (newParams, files)})
          pure paramValue'

formParamList :: forall a m. (Web.FromHttpApiData a, RequestM m) => BS.ByteString -> m (NonEmpty.NonEmpty a)
formParamList = Combinators.NonEmpty.some . formParam

-- | Parse a single form file
formFile :: RequestM m => BS.ByteString -> m (WAI.FileInfo LBS.ByteString)
formFile paramName = do
  body' <- body
  case body' of
    Request.Raw _ -> Failure.next
    Request.Multipart (params, files) -> do
      case lookup paramName files of
        Nothing -> Failure.next
        Just fileInfo -> do
          let newFiles = deleteFile paramName files
          Request.modify (\request -> request {Request.body = Request.Multipart (params, newFiles)})
          pure fileInfo
  where
    deleteFile :: BS.ByteString -> [WAI.File LBS.ByteString] -> [WAI.File LBS.ByteString]
    deleteFile paramName [] = []
    deleteFile paramName (f@(paramName', fileInfo) : fs) =
      if paramName == paramName'
        then fs
        else f : deleteFile paramName fs

bodyEnd :: RequestM m => m ()
bodyEnd = do
  currentBody <- body
  case currentBody of
    Request.Raw (LBS.null -> True) -> pure ()
    Request.Multipart ([], []) -> pure ()
    _ -> Failure.next

-- $headerParsers
--
-- These are header parsers.

headers :: RequestM m => m Request.Headers
headers = do
  headers <- Request.gets Request.headers
  Request.modify (\request -> request {Request.headers = []})
  pure headers

header :: RequestM m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  maybeHeader <- Request.gets (Foldable.find (\(headerName', _) -> headerName == headerName') . Request.headers)
  case maybeHeader of
    Nothing -> Failure.next
    Just header@(_, headerValue) -> do
      Request.modify (\request -> request {Request.headers = List.delete header $ Request.headers request})
      pure headerValue

headersEnd :: RequestM m => m ()
headersEnd = do
  currentHeaders <- headers
  if List.null currentHeaders
    then pure ()
    else Failure.next

cookie :: RequestM m => m Request.Cookie
cookie = do
  cookieValue <- header "Cookie"
  pure $ Web.parseCookies cookieValue

cookieCrumb :: RequestM m => BS.ByteString -> m BS.ByteString
cookieCrumb name = do
  cookieValue <- cookie
  case List.lookup name cookieValue of
    Nothing -> Failure.next
    Just crumbValue -> do
      let crumb = (name, crumbValue)
      -- TODO: Needs testing to see if state is restored properly
      Request.modify (\request -> request {Request.headers = ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete crumb cookieValue) : Request.headers request})
      pure crumbValue

cookieEnd :: RequestM m => m ()
cookieEnd = do
  currentCookie <- cookie
  if List.null currentCookie
    then pure ()
    else Failure.next

basicAuth :: RequestM m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case Text.decodeBase64 encodedCreds of
        Left _ -> Failure.next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> Failure.next
    _ -> Failure.next

-- $vaultParsers

vault :: RequestM m => m Vault.Vault
vault = Request.gets Request.vault

vaultLookup :: RequestM m => Vault.Key a -> m a
vaultLookup key = do
  vault <- Request.gets Request.vault
  maybe Failure.next pure (Vault.lookup key vault)

vaultInsert :: RequestM m => Vault.Key a -> a -> m ()
vaultInsert key value = do
  vault <- Request.gets Request.vault
  Request.modify (\request -> request {Request.vault = Vault.insert key value vault})

vaultDelete :: RequestM m => Vault.Key a -> m ()
vaultDelete key = do
  vault <- Request.gets Request.vault
  Request.modify (\request -> request {Request.vault = Vault.delete key vault})

vaultAdjust :: RequestM m => (a -> a) -> Vault.Key a -> m ()
vaultAdjust adjuster key = do
  vault <- Request.gets Request.vault
  Request.modify (\request -> request {Request.vault = Vault.adjust adjuster key vault})

vaultWipe :: RequestM m => m ()
vaultWipe = Request.modify (\request -> request {Request.vault = Vault.empty})
