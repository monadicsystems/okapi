# 🦓🦒Okapi

Okapi is a data-driven micro framework for implementing HTTP servers.

- Ergonomic DSLs for routing and parsing requests
- Integrate Okapi with ANY monad stack or effect system
- Automatically generate clients and OpenAPI specifications (coming soon)
- Programatically generate your API's structure

```haskell
helloWorld =
  responder @200 @'[] @Text.Text @Text.Text
    . method HTTP.GET id
    $ \greet _req -> do
      return $ greet noHeaders "Hello World!"

main =
  Warp.run 8000
    . withDefault helloWorld
    $ \_ resp ->
      resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."
```
