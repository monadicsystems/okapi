---
title: Servant integration
description: A guide on integrating Okapi with a Servant application.
---

Okapi and Servant make a great combo. Since Okapi is built directly on top of [wai](https://hackage.haskell.org/package/wai),
you can embed your Okapi application into Servant easily. Here's how.

---

1. Define your Servant API type and handlers as usual:

    ```haskell
    type ServantAPI = Get '[JSON] MyData

    servantHandler :: Server ServantAPI
    servantHandler = pure MyData
    ```

2. Turn your Okapi API into an `Application` using `makeOkapiApp`:

    ```haskell
    okapiApp :: Application
    okapiApp = makeOkapiApp id okapiAPI

    okapiAPI :: OkapiT IO Response
    okapiAPI = do
      ...
    ```

2. Wrap your Okapi app with the `Tagged` data constructor from `Data.Tagged` and annotate it with the type `Tagged Handler Application`:

    ```haskell
    taggedOkapiApp :: Tagged Handler Application
    taggedOkapiApp = Tagged okapiApp
    ```

3. Add the `Raw` type from `Servant.API.Raw` to your Servant API type and create a `Proxy` value for it:

    ```haskell
    type API = ServantAPI :<|> Raw

    apiProxy :: Proxy API
    apiProxy = Proxy
    ```

4. Add your tagged Okapi app to the Servant handler in the same spot where `Raw` is in the API type:

    ```haskell
    apiHandler :: Server API
    apiHandler = servantHandler :<|> taggedOkapiApp
    ```

5. Serve your Servant app as usual:

    ```haskell
    main :: IO ()
    main = do
        print "Running app on port 8080"
        run 8080 $ serve apiProxy apiHandler
    ```

All together, it should look something like this:

```haskell
main :: IO ()
main = do
    print "Running app on port 8080"
    run 8080 $ serve apiProxy apiHandler

-- Servant + Okapi
type API = ServantAPI :<|> Raw

apiHandler :: Server API
apiHandler = servantHandler :<|> taggedOkapiApp

apiProxy :: Proxy API
apiProxy = Proxy

-- Servant
type ServantAPI = Get '[JSON] MyData

servantHandler :: Server ServantAPI
servantHandler = pure MyData

-- Okapi
taggedOkapiApp :: Tagged Handler Application
taggedOkapiApp = Tagged okapiApp

okapiApp :: Application
okapiApp = makeOkapiApp id okapiAPI

okapiAPI :: OkapiT IO Response
okapiAPI = do
  ...
```

You can find a complete example [here](https://github.com/monadicsystems/okapi/tree/main/examples/servant).

{% callout title="Warning" type="warning" %}
  Beware if you are routing to your Okapi app with `:>` like so:

  ```haskell
  apiHandler :: Server API
  apiHandler =
    servantHandler
    :<|> ("blah" :> "okapi" :> taggedOkapiApp)
  ```

  This will strip away `pathInfo` as mentioned in the Haddock documentation for [Servant.API.Raw](https://hackage.haskell.org/package/servant-0.19/docs/Servant-API-Raw.html),
  so adjust your Okapi app accordingly.

  If your Okapi app is defined as:

  ```haskell
  okapiAPI = do
      methodGET
      pathParam `is` "blah"
      pathParam `is` "okapi"
      ...
  ```

  Servant will parse the path segments `blah` and `okapi` before passing the request to your Okapi app,
  so you only need to have:

  ```haskell
  okapiAPI = do
      methodGET
      ...
  ```
  
  According to the docs, only the request path is affected.
{% /callout %}

{% callout title="Gradually Typing Your APIs" %}
  The ability to easily embed your Okapi APIs into Servant makes it suitable as quick prototyping tool.
  You can start of with an API that is mostly implemented in Okapi, which is easier to use and modify quickly, but isn't as type safe:

  ```haskell
  type API = TypedEndpoint1 :<|> Raw

  apiHandler :: Server API
  apiHandler = typedEndpoint1Handler :<|> taggedBigOkapiApp
  ```

  And gradually move more endpoints from your Okapi API into your Servant API:

  ```haskell
  type API =
      TypedEndpoint1
      :<|> TypedEndpoint2
      :<|> TypedEndpoint3
      :<|> TypedEndpoint4
      :<|> TypedEndpoint5
      :<|> Raw

  apiHandler :: Server API
  apiHandler =
      typedEndpoint1Handler
      :<|> typedEndpoint2Handler
      :<|> typedEndpoint3Handler
      :<|> typedEndpoint4Handler
      :<|> typedEndpoint5Handler
      :<|> taggedSmallerOkapiApp
  ```
{% /callout %}
