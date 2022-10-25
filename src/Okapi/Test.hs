module Okapi.Test where

-- $testing
--
-- There are two ways to test in Okapi.

{-
test ::
  Monad m =>
  ServerT m Response ->
  Request ->
  m (Either Failure Response, State)
test serverT request =
  (State.runStateT . Except.runExceptT . runServerT $ serverT)
    (requestToState request)
  where
    requestToState :: Request -> State
    requestToState stateRequest = let stateVault = mempty in State {..}

testPure ::
  ServerT Identity.Identity Response ->
  Request ->
  Identity.Identity (Either Failure Response, State)
testPure = test

testIO ::
  ServerT IO Response ->
  Request ->
  IO (Either Failure Response, State)
testIO = test

-- TODO: Add common assertion helpers. Use Predicate for Contravariant interface??

assert ::
  ((Either Failure Response, State) -> Bool) ->
  (Either Failure Response, State) ->
  Bool
assert assertion = assertion

assert200 :: (Either Failure Response, State) -> Bool
assert200 = \case
  (Right (Response 200 _ _), _) -> True
  _ -> False

assert404 :: (Either Failure Response, State) -> Bool
assert404 = \case
  (Right (Response 404 _ _), _) -> True
  _ -> False

assert500 :: (Either Failure Response, State) -> Bool
assert500 = \case
  (Right (Response 500 _ _), _) -> True
  _ -> False

testRunSession ::
  Monad m =>
  WAI.Session a ->
  (forall a. m a -> IO a) ->
  ServerT m Response ->
  IO a
testRunSession session hoister serverT = do
  let waiApp = app notFound hoister serverT
  WAI.runSession session waiApp

testWithSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  ServerT m Response ->
  WAI.Session a ->
  IO a
testWithSession hoister serverT session = testRunSession session hoister serverT

testRequest :: Request -> WAI.Session WAI.SResponse
testRequest = WAI.srequest . requestToSRequest
  where
    requestToSRequest :: Request -> WAI.SRequest
    requestToSRequest request@(Request mbMethod path query body headers) =
      let requestMethod = Maybe.fromMaybe HTTP.methodGet mbMethod
          sRequestBody =
            case body of
              BodyRaw lbs -> lbs
              BodyMultipart _ -> error "Must use BodyRaw for testRequest"
          rawPath = RelURL path query Function.& \relURL -> Text.encodeUtf8 $ renderRelURL relURL
          sRequestRequest = WAI.setPath (WAI.defaultRequest {WAI.requestMethod = requestMethod, WAI.requestHeaders = headers}) rawPath
       in WAI.SRequest sRequestRequest sRequestBody
-}
