{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Test where

import qualified Control.Monad.Except as ExceptT
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Strict as StateT
import Data.ByteString.Internal as BS
import Data.ByteString.Lazy.Internal as LBS
import Data.Function
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Test (setRawPathInfo, SRequest (..))
import qualified Network.Wai.Test as Wai.Test
import qualified Okapi
import qualified Okapi.State as Okapi
import Network.HTTP.Types (decodePath, queryToQueryText)
import qualified Data.Bifunctor
import Network.Wai (defaultRequest)

data TestRequest = TestRequest
  { testRequestMethod :: HTTP.Method,
    testRequestHeaders :: HTTP.RequestHeaders,
    testRequestRawPath :: BS.ByteString,
    testRequestBody :: LBS.ByteString
  }

testParser ::
  Monad m =>
  (forall a. m a -> IO a) ->
  Okapi.OkapiT m Okapi.Response ->
  TestRequest ->
  IO (Either Okapi.Failure Okapi.Response, Okapi.State)
testParser hoister okapiT testRequest =
  (StateT.runStateT . ExceptT.runExceptT . Okapi.unOkapiT $ Morph.hoist hoister okapiT)
    (testRequestToState testRequest)

testParserIO :: Okapi.OkapiT IO Okapi.Response -> TestRequest -> IO (Either Okapi.Failure Okapi.Response, Okapi.State)
testParserIO = testParser id

testRequestToState :: TestRequest -> Okapi.State
testRequestToState (TestRequest method headers rawPath body) =
  let requestMethod = method
      (requestPath, requestQuery) = Data.Bifunctor.second queryToQueryText $ decodePath rawPath
      requestBody = pure body
      requestHeaders = headers
      requestVault = mempty
      stateRequest = Okapi.Request {..}
      stateRequestMethodParsed = False
      stateRequestBodyParsed = False
      stateResponded = False
    in Okapi.State {..}

-- ASSERTION FUNCTIONS

assertFailure ::
  (Okapi.Failure -> Bool) ->
  (Either Okapi.Failure Okapi.Response, Okapi.State) ->
  Bool
assertFailure assertion parserResult = case parserResult of
  (Left failure, _) -> assertion failure
  _ -> False

assertResponse ::
  (Okapi.Response -> Bool) ->
  (Either Okapi.Failure Okapi.Response, Okapi.State) ->
  Bool
assertResponse assertion parserResult = case parserResult of
  (Right response, _) -> assertion response
  _ -> False

assertState ::
  (Okapi.State -> Bool) ->
  (Either Okapi.Failure Okapi.Response, Okapi.State) ->
  Bool
assertState assertion (_, parserResultState) = assertion parserResultState

-- BELOW IS FOR USE WITH WAI TEST

testRequestToSRequest :: TestRequest -> Wai.Test.SRequest
testRequestToSRequest (TestRequest method headers rawPath body) =
  let requestMethod = method
      sRequestBody = body
      sRequestRequest = Wai.Test.setPath (defaultRequest { Wai.requestMethod = method, Wai.requestHeaders = headers }) rawPath
    in SRequest sRequestRequest sRequestBody

runSession ::
  Monad m =>
  Wai.Test.Session a ->
  (forall a. m a -> IO a) ->
  Okapi.OkapiT m Okapi.Response ->
  IO a
runSession session hoister okapiT = do
  let app = Okapi.makeOkapiApp hoister Okapi.notFound okapiT
  Wai.Test.runSession session app

withSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  Okapi.OkapiT m Okapi.Response ->
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
