{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Test
  ( TestRequest (..),
    testParser,
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

import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Strict as State
import qualified Data.Bifunctor
import Data.ByteString.Internal as BS
import Data.ByteString.Lazy.Internal as LBS
import Data.Function
import Network.HTTP.Types (decodePath, queryToQueryText)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (defaultRequest)
import qualified Network.Wai as Wai
import Network.Wai.Test (SRequest (..), setRawPathInfo)
import qualified Network.Wai.Test as Wai.Test
import Okapi.Application
import Okapi.Response
import Okapi.Types

data TestRequest = TestRequest
  { testRequestMethod :: HTTP.Method,
    testRequestHeaders :: HTTP.RequestHeaders,
    testRequestRawPath :: BS.ByteString,
    testRequestBody :: LBS.ByteString
  }

testParser ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  TestRequest ->
  IO (Either Failure Response, State)
testParser hoister okapiT testRequest =
  (State.runStateT . Except.runExceptT . unOkapiT $ Morph.hoist hoister okapiT)
    (testRequestToState testRequest)

testParserIO ::
  OkapiT IO Response ->
  TestRequest ->
  IO (Either Failure Response, State)
testParserIO = testParser id

testRequestToState :: TestRequest -> State
testRequestToState (TestRequest method headers rawPath body) =
  let requestMethod = method
      (requestPath, requestQuery) = Data.Bifunctor.second queryToQueryText $ decodePath rawPath
      requestBody = pure body
      requestHeaders = headers
      requestVault = mempty
      stateRequest = Request {..}
      stateRequestMethodParsed = False
      stateRequestBodyParsed = False
      stateResponded = False
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
  let app = okapiApp hoister _404 okapiT
  Wai.Test.runSession session app

withSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  Wai.Test.Session a ->
  IO a
withSession hoister okapiT session = runSession session hoister okapiT

testRequest :: TestRequest -> Wai.Test.Session Wai.Test.SResponse
testRequest TestRequest {..} =
  let request =
        Wai.defaultRequest
          { Wai.requestMethod = testRequestMethod,
            Wai.requestHeaders = testRequestHeaders
          }
      simpleRequest = Wai.Test.setPath request testRequestRawPath
      simpleRequestBody = testRequestBody
      finalRequest = Wai.Test.SRequest simpleRequest simpleRequestBody
   in Wai.Test.srequest finalRequest

testRequestToSRequest :: TestRequest -> Wai.Test.SRequest
testRequestToSRequest (TestRequest method headers rawPath body) =
  let requestMethod = method
      sRequestBody = body
      sRequestRequest = Wai.Test.setPath (defaultRequest {Wai.requestMethod = method, Wai.requestHeaders = headers}) rawPath
   in SRequest sRequestRequest sRequestBody
