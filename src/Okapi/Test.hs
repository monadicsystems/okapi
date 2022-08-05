{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Test where

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
import Okapi.Internal.Functions.Application
import Okapi.Internal.Types
import Okapi.Response

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

-- ASSERTION FUNCTIONS

assertFailure ::
  (Failure -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assertFailure assertion parserResult = case parserResult of
  (Left failure, _) -> assertion failure
  _ -> False

assertResponse ::
  (Response -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assertResponse assertion parserResult = case parserResult of
  (Right response, _) -> assertion response
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
  let app = makeOkapiApp hoister notFound okapiT
  Wai.Test.runSession session app

withSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  Wai.Test.Session a ->
  IO a
withSession hoister okapiT session = runSession session hoister okapiT

send :: TestRequest -> Wai.Test.Session Wai.Test.SResponse
send TestRequest {..} =
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
