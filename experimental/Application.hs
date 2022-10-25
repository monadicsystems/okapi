{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module exports functions for turning HTTP request parsers into WAI applications.
module Okapi.Application
  ( app,
    Okapi.Application.websocketsApp,
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Strict as State
import qualified Data.Map as Map
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.WebSockets as WS
import Okapi.Event
import Okapi.Types

-- | Turns a parser into a WAI application
app ::
  Monad m =>
  -- | Function for "unlifting" monad inside @ServerT@ to @IO@ monad
  (forall a. m a -> IO a) ->
  -- | The default response to return if parser fails
  Response ->
  -- | The parser used to match the request
  ServerT m Response ->
  Wai.Application
app hoister defaultResponse okapiT waiRequest respond = do
  state <- waiRequestToState waiRequest
  (eitherFailureOrResponse, _state) <- (State.runStateT . Except.runExceptT . runServerT $ Morph.hoist hoister okapiT) state
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

    waiRequestToState :: Wai.Request -> IO State
    waiRequestToState waiRequest = do
      requestBody <- Wai.strictRequestBody waiRequest
      let requestMethod = Wai.requestMethod waiRequest
          requestPath = Wai.pathInfo waiRequest
          requestQuery = map (\case (name, Nothing) -> (name, QueryFlag); (name, Just txt) -> (name, QueryParam txt)) $ HTTP.queryToQueryText $ Wai.queryString waiRequest
          requestHeaders = Wai.requestHeaders waiRequest
          stateRequest = Request {..}
          stateRequestMethodParsed = False
          stateRequestBodyParsed = False
          stateResponded = False
          stateVault = Wai.vault waiRequest

      pure State {..}

-- | Turns a parsers into a WAI application with WebSocket functionality
-- See __ for information on how to create a WebSocket server
websocketsApp ::
  Monad m =>
  (forall a. m a -> IO a) ->
  Response ->
  ServerT m Response ->
  -- | Connection options configuration for the WebSocket server
  WS.ConnectionOptions ->
  -- | The server to use for handling WebSocket connections
  WS.ServerApp ->
  Wai.Application
websocketsApp hoister defaultResponse okapiT connSettings serverApp =
  let backupApp = app hoister defaultResponse okapiT
   in WS.websocketsOr connSettings serverApp backupApp
