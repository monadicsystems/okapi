---
---

# Okapi

```haskell
data App where
  Match :: forall a. (Web.ToHttpApiData a) => a -> [App] -> App
  Param :: forall a. (Web.FromHttpApiData a, Typeable.Typeable a) => (Secret.Secret a -> [App]) -> App
  Regex :: forall a. Text -> (Secret.Secret a -> [App]) -> App
  Splat :: forall a. (Web.FromHttpApiData a) => (Secret.Secret (NonEmpty.NonEmpty a) -> [App]) -> App
  Route :: forall a. Route.Parser a -> (Secret.Secret a -> [App]) -> App
  Method :: forall env. HTTP.StdMethod -> (env Natural.~> IO) -> Handler env -> App
  Query :: forall a. Query.Parser a -> (Secret.Secret a -> [App]) -> App
  Headers :: forall a. RequestHeaders.Parser a -> (Secret.Secret a -> [App]) -> App
  -- Body :: forall a. RequestBody.Parser a -> (Secret.Secret a -> [App]) -> App
  Pipe :: Wai.Middleware -> App -> App
  Respond ::
    forall (status :: Natural.Natural) (headerKeys :: [Exts.Symbol]) (contentType :: Type) (resultType :: Type).
    (Response.ToContentType contentType resultType) =>
    ((Response.Headers headerKeys -> resultType -> Wai.Response) -> [App]) ->
    App
-- Endpoint :: HTTP.StdMethod -> Route.Parser a -> (env Natural.~> IO) -> App
```

## `endpoint` function

```haskell
endpoint
  :: HTTP.StdMethod
  -> Route.Parser a
  -> (env Natural.~> IO)
  -> (Secret.Secret a -> Handler env)
  -> App
endpoint stdMethod routeP trans handlerWithSecret = route routeP \routeS ->
  [ method stdMethod trans (handlerWithSecret routeS)
  ]

myAPI :: [App]
myAPI =
  [ endpoint GET (do Route.lit "user";) id \_ req -> do
      ...
  , endpoint POST (do Route.lit "user"; Route.param @UserID;) id \userIDS req -> do
      let userID = Secret.tell req userIDs
      ...
  ]
```

## `on` function

```haskell
on :: Operation a -> (Secret.Secret a -> [App]) -> App
on op ... = 

myAPI =
  [on|/api|]
    [ [on|/v2|]
        [ [on|?name:Text|] \nameS ->
            [ getIO \req -> do
                ...
            ]
        , [on|/:Text|] \nameS ->
            [ getIO \req -> do
                ...
            ]
        , [on|/*Int|] \intsS ->
            [ method GET id \req -> do
                ...
            ]
        , [on|{ Accept:Text, XSRF-Token:XSRFToken }|] \headersS ->
            [ [on|POST /new/:Int|] id \intS req -> do
                ...
            ]
        ]
    ]
```