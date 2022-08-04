{-# LANGUAGE RankNTypes #-}

module Okapi.Internal.Functions.Application where

makeOkapiApp :: Monad m => (forall a. m a -> IO a) -> Response -> OkapiT m Response -> Wai.Application
makeOkapiApp hoister defaultResponse okapiT waiRequest respond = do
  (eitherFailureOrResponse, _state) <- (StateT.runStateT . ExceptT.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) (waiRequestToState waiRequest)
  let response =
        case eitherFailureOrResponse of
          Left Skip -> defaultResponse
          Left (Error errorResponse) -> errorResponse
          Right succesfulResponse -> succesfulResponse
  responseToWaiApp response waiRequest respond

makeOkapiAppWebsockets :: Monad m => (forall a. m a -> IO a) -> Response -> OkapiT m Response -> ConnectionOptions -> ServerApp -> Wai.Application
makeOkapiAppWebsockets hoister defaultResponse okapiT connSettings serverApp =
  let backup = makeOkapiApp hoister defaultResponse okapiT
  in websocketsOr connSettings serverApp backup

responseToWaiApp :: Response -> Wai.Application
responseToWaiApp (Response {..}) waiRequest respond = case responseBody of
  ResponseBodyRaw body -> respond $ Wai.responseLBS (toEnum $ fromEnum responseStatus) responseHeaders body
  ResponseBodyFile filePath -> respond $ Wai.responseFile (toEnum $ fromEnum responseStatus) responseHeaders filePath Nothing
  ResponseBodyEventSource eventSource -> (gzip def $ Event.eventSourceAppUnagiChan eventSource) waiRequest respond

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
