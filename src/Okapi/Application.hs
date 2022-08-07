{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module exports functions for turning HTTP request parsers into WAI applications.
module Okapi.Application
  ( okapiApp,
    okapiAppWebsockets,
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Strict as State
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.WebSockets as WS
import Okapi.Event
import Okapi.Types

-- | Turns a parser into a WAI application
okapiApp ::
  Monad m =>
  -- | Function for "unlifting" monad inside @OkapiT@ to @IO@ monad
  (forall a. m a -> IO a) ->
  -- | The default response to return if parser fails
  Response ->
  -- | The parser used to match the request
  OkapiT m Response ->
  Wai.Application
okapiApp hoister defaultResponse okapiT waiRequest respond = do
  (eitherFailureOrResponse, _state) <- (State.runStateT . Except.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) (waiRequestToState waiRequest)
  let response =
        case eitherFailureOrResponse of
          Left Skip -> defaultResponse
          Left (Error errorResponse) -> errorResponse
          Right succesfulResponse -> succesfulResponse
  responseToWaiApp response waiRequest respond
  where
    responseToWaiApp :: Response -> Wai.Application
    responseToWaiApp (Response {..}) waiRequest respond = case responseBody of
      ResponseBodyRaw body -> respond $ Wai.responseLBS (toEnum $ fromEnum responseStatus) responseHeaders body
      ResponseBodyFile filePath -> respond $ Wai.responseFile (toEnum $ fromEnum responseStatus) responseHeaders filePath Nothing
      ResponseBodyEventSource eventSource -> (Middleware.gzip Middleware.def $ eventSourceAppUnagiChan eventSource) waiRequest respond

    waiRequestToState :: Wai.Request -> State
    waiRequestToState waiRequest =
      let requestMethod = Wai.requestMethod waiRequest
          requestPath = Wai.pathInfo waiRequest
          requestQuery = HTTP.queryToQueryText $ Wai.queryString waiRequest
          requestBody = Wai.strictRequestBody waiRequest
          requestHeaders = Wai.requestHeaders waiRequest
          requestVault = Wai.vault waiRequest
          stateRequest = Request {..}
          stateRequestMethodParsed = False
          stateRequestBodyParsed = False
          stateResponded = False
       in State {..}

-- | Turns a parsers into a WAI application with WebSocket functionality
-- See __ for information on how to create a WebSocket server
okapiAppWebsockets ::
  Monad m =>
  (forall a. m a -> IO a) ->
  Response ->
  OkapiT m Response ->
  -- | Connection options configuration for the WebSocket server
  WS.ConnectionOptions ->
  -- | The server to use for handling WebSocket connections
  WS.ServerApp ->
  Wai.Application
okapiAppWebsockets hoister defaultResponse okapiT connSettings serverApp =
  let backup = okapiApp hoister defaultResponse okapiT
   in WS.websocketsOr connSettings serverApp backup
