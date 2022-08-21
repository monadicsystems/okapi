{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Okapi.Test
  ( testParser,
    testParserIO,
    assertFailure,
    assertState,
    assertResponse,
    -- For use with Wai.Test
    runSession,
    withSession,
    testRequest,
    -- Assertion Helpers
    is200,
    is404,
    is500,
    isSkip,
    hasBodyRaw,
  )
where

import Control.Monad.Combinators
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Strict as State
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor
import Data.ByteString.Internal as BS
import Data.ByteString.Lazy.Internal as LBS
import Data.Either.Extra
import Data.Function
import qualified Data.Map as Map
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (decodePath, queryToQueryText)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (defaultRequest)
import qualified Network.Wai as Wai
import Network.Wai.Test (SRequest (..), setRawPathInfo)
import qualified Network.Wai.Test as Wai.Test
import Okapi.Application
import Okapi.Response
import Okapi.Types

request :: Method -> URL -> Body -> Headers -> Maybe Request
request method url body headers = case parseURL url of
  Nothing -> Nothing
  Just (path, query) -> Just $ Request method path query body headers

parseURL :: URL -> Maybe (Path, Query)
parseURL (URL url) = eitherToMaybe $
  flip Atto.parseOnly url $ do
    path <- many pathSeg
    maybeQueryStart <- optional $ Atto.char '?'
    case maybeQueryStart of
      Nothing -> pure (path, [])
      Just _ -> do
        query <- many queryParam
        pure (path, query)
  where
    pathSeg :: Atto.Parser Text
    pathSeg = do
      Atto.char '/'
      Atto.takeWhile (\c -> c /= '/' && c /= '?')

    queryParam :: Atto.Parser (Text, QueryValue)
    queryParam = do
      queryParamName <- Atto.takeWhile (\c -> c /= '=' && c /= '&')
      mbEquals <- optional $ Atto.char '='
      case mbEquals of
        Nothing -> pure (queryParamName, QueryFlag)
        Just _ -> do
          queryParamValue <- Atto.takeWhile (/= '&')
          pure (queryParamName, QueryParam queryParamValue)

requestURL :: Request -> URL
requestURL (Request _ path query _ _) = url path query

url :: Path -> Query -> URL
url [] [] = ""
url [] query = "?" <> queryToURL query
url path [] = pathToURL path
url path query = pathToURL path <> "?" <> queryToURL query

queryToURL :: Query -> URL
queryToURL [] = ""
queryToURL ((name, QueryFlag) : query) = URL name <> "&" <> queryToURL query
queryToURL ((name, QueryParam value) : query) = URL name <> "=" <> URL value <> "&" <> queryToURL query

pathToURL :: Path -> URL
pathToURL [] = ""
pathToURL (pathSeg : path) = "/" <> URL pathSeg <> pathToURL path

testParser ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  Request ->
  IO (Either Failure Response, State)
testParser hoister okapiT request =
  (State.runStateT . Except.runExceptT . unOkapiT $ Morph.hoist hoister okapiT)
    (requestToState request)

testParserIO ::
  OkapiT IO Response ->
  Request ->
  IO (Either Failure Response, State)
testParserIO = testParser id

requestToState :: Request -> State
requestToState stateRequest =
  let stateRequestMethodParsed = False
      stateRequestBodyParsed = False
      stateResponded = False
      stateVault = mempty
   in State {..}

-- ASSERTION FUNCTIONS TODO: Add common assertion helpers

assertFailure ::
  (Failure -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assertFailure assertion parserResult = case parserResult of
  (Left failure, _) -> assertion failure
  _ -> False

isSkip :: Failure -> Bool
isSkip Skip = True
isSkip _ = False

assertResponse ::
  (Response -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assertResponse assertion parserResult = case parserResult of
  (Right response, _) -> assertion response
  _ -> False

is200 :: Response -> Bool
is200 Response {..} = responseStatus == 200

is404 :: Response -> Bool
is404 Response {..} = responseStatus == 404

is500 :: Response -> Bool
is500 Response {..} = responseStatus == 500

hasBodyRaw :: LBS.ByteString -> Response -> Bool
hasBodyRaw match Response {..} = case responseBody of
  ResponseBodyRaw bs -> bs == match
  _ -> False

assertState ::
  (State -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assertState assertion (_, parserResultState) = assertion parserResultState

-- BELOW IS FOR USE WITH WAI TEST

runSession ::
  Monad m =>
  Wai.Test.Session a ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO a
runSession session hoister okapiT = do
  let waiApp = app hoister notFound okapiT
  Wai.Test.runSession session waiApp

withSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  Wai.Test.Session a ->
  IO a
withSession hoister okapiT session = runSession session hoister okapiT

testRequest :: Request -> Wai.Test.Session Wai.Test.SResponse
testRequest = Wai.Test.srequest . requestToSRequest

requestToSRequest :: Request -> Wai.Test.SRequest
requestToSRequest request@(Request method path query body headers) =
  let requestMethod = method
      sRequestBody = body
      rawPath = requestURL request & \(URL url) -> encodeUtf8 url
      sRequestRequest = Wai.Test.setPath (defaultRequest {Wai.requestMethod = method, Wai.requestHeaders = headers}) rawPath
   in SRequest sRequestRequest sRequestBody
