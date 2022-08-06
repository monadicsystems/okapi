{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Parser
  ( -- Method parsers
    get,
    post,
    head,
    put,
    delete,
    trace,
    connect,
    options,
    anyMethod,
    method,
    -- Path parsers
    pathSeg,
    path,
    pathParam,
    pathParamRaw,
    pathSegWith,
    -- Query parsers
    queryParam,
    queryParamRaw,
    queryFlag,
    -- Header parsers
    basicAuth,
    cookies,
    header,
    -- Body parsers
    bodyJSON,
    bodyForm,
    bodyRaw,
    -- Response Helpers
    respond,
    -- Error Helpers
    skip,
    throw,
    (<!>),
    guardThrow,
    optionalThrow,
    optionThrow,
    -- State Checkers
    methodParsed,
    pathParsed,
    queryParsed,
    headersParsed,
    bodyParsed,
  )
where

import Control.Monad.Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Base64
import qualified Network.HTTP.Types as HTTP
import Okapi.Types
import qualified Web.Cookie as Cookie
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (head)

-- METHOD HELPERS

get :: forall m. MonadOkapi m => m ()
get = method HTTP.methodGet

post :: forall m. MonadOkapi m => m ()
post = method HTTP.methodPost

head :: forall m. MonadOkapi m => m ()
head = method HTTP.methodHead

put :: forall m. MonadOkapi m => m ()
put = method HTTP.methodPut

delete :: forall m. MonadOkapi m => m ()
delete = method HTTP.methodDelete

trace :: forall m. MonadOkapi m => m ()
trace = method HTTP.methodTrace

connect :: forall m. MonadOkapi m => m ()
connect = method HTTP.methodConnect

options :: forall m. MonadOkapi m => m ()
options = method HTTP.methodOptions

patch :: forall m. MonadOkapi m => m ()
patch = method HTTP.methodPatch

anyMethod :: forall m. MonadOkapi m => m ()
anyMethod = parseMethod >> pure ()

method :: forall m. MonadOkapi m => HTTP.Method -> m ()
method method = do
  method' <- parseMethod
  if method == method'
    then pure ()
    else skip

-- PATH HELPERS

-- | Parses a single path segment matching the given text and discards it
pathSeg :: forall m. MonadOkapi m => Text.Text -> m ()
pathSeg goal = pathSegWith (goal ==)

-- | Parses mutiple segments matching the order of the given list and discards them
-- | TODO: Needs testing. May not have the correct behavior
path :: forall m. MonadOkapi m => [Text.Text] -> m ()
path = mapM_ pathSeg

-- | Parses a single seg segment, and returns the parsed seg segment as a value of the given type
pathParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => m a
pathParam = do
  pathSeg <- parsePathSeg
  maybe skip pure (Web.parseUrlPieceMaybe pathSeg)

pathParamRaw :: forall m. MonadOkapi m => m Text.Text
pathParamRaw = parsePathSeg

pathSegWith :: forall m. MonadOkapi m => (Text.Text -> Bool) -> m ()
pathSegWith predicate = do
  pathSeg <- parsePathSeg
  if predicate pathSeg
    then pure ()
    else skip

-- QUERY HELPERS

queryParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  maybe skip pure (Web.parseQueryParamMaybe =<< queryItemValue)

queryParamRaw :: forall m. MonadOkapi m => Text.Text -> m Text.Text
queryParamRaw queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  maybe skip pure queryItemValue

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

cookies :: forall m. MonadOkapi m => m Cookies
cookies = do
  cookiesValue <- header "Cookie"
  pure $ Cookie.parseCookiesText cookiesValue

-- TODO: Any checks required??
header :: forall m. MonadOkapi m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  (_headerName, headerValue) <- parseHeader headerName
  pure headerValue

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
  check3 <- queryParsed
  if check1 && check2 && check3 then return response else skip

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

parseHeader :: MonadOkapi m => HTTP.HeaderName -> m HTTP.Header
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
      bodyRef <- State.gets (requestBody . stateRequest)
      body <- IO.liftIO bodyRef
      State.modify (\state -> state {stateRequestBodyParsed = True})
      pure body

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
