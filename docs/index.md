# 🦓🦒Okapi

Okapi is a micro framework for HTTP servers.

- Ergonomic DSL for parsing requests
- Integrates seamlessly with ANY monad stack or effect system
- Automagically generate OpenAPI specifications (clients coming soon)
- Lightweight, minimal abstraction built on top of WAI

**Okapi is NOT recommended for production use at this time.**

Please feel free to reach out to me on [GitHub](https://github.com/monadicsystems/okapi) if you have any questions/criticisms/ideas or would like to contribute to this project.

## Getting Started

1. Use the command `stack new <project-name>` to create a new Haskell project
2. Add the `okapi` library to your project's dependencies

## Introduction

There are two ways to implement *Servers* in Okapi.

The recommended way to implement a Server in Okapi is via *Endpoints*:

```haskell
-- | Define Endpoints using an Applicative eDSL
myEndpoint = Endpoint
  { method = GET
  , path = do
      Path.static "index"
      magicNumber <- Path.param @Int
      pure magicNumber
  , query = do
      x <- Query.param @Int "x"
      y <- Query.option 10 $ Query.param @Int "y"
      pure (x, y)
  , body = do
      foo <- Body.json @Value
      pure foo
  , headers = pure ()
  , responder = do
      itsOk <- Responder.json @Int status200 do
        addSecretNumber <- AddHeader.using @Int "X-SECRET"
        pure addSecretNumber
      pure itsOk
  }
```

An alternative, more concise way of defining a Server in Okapi is via *Matchpoints*:

```haskell
-- | Define Matchpoint patterns using PatternSynonyms,
--   ViewPatterns, and the same eDSL used for Endpoints
pattern GetUsers optF <- Matchpoint
  GET
  ["users"]
  _
  (Body.eval $ Body.optional $ Body.json @Filter -> Ok optF)
  _

pattern AddUser user <- Matchpoint
  POST                                     -- Method
  ["users"]                                -- Path
  _                                        -- Query
  (Body.eval $ Body.json @User -> Ok user) -- Body
  _                                        -- Headers

pattern GetUsersByID userID <- Matchpoint
  GET
  (Path.eval $ Path.static "users" *> Path.param @UserID -> Ok userID)
  _
  ""
  _

-- | Servers are just contextful functions from a Request to a Response
type Server m = Request -> m Response

myServer :: Server IO
myServer = \case
  GetUser -> do
    ...
  GetUserByID userID -> do
    ...
  AddUser user -> do
    ...
  _ -> do
    ...

-- | Run your Server using Warp
main = Warp.run 3000 $ instantiate id myServer
```

The advantadge of using Endpoints over Matchpoints is that Okapi can
automatically generate *Specifications* and *Clients* for a Server implemented
with Endpoints, but not a Server implemented with Matchpoints.

On the flip side, a Server implemented with Matchpoints will be more
concise than the same Server implemented with Endpoints.

## Endpoint

An Endpoint is an *executable specification* representing a single Operation that can be taken against your API.

An Endpoint has 6 fields.

```haskell
data Endpoint p q h b r = Endpoint
  { method :: StdMethod             -- (1)
  , path :: Path.Script p           -- (2)
  , query :: Query.Script q         -- (3)
  , body :: Body.Script b           -- (4)
  , headers :: Headers.Script h     -- (5)
  , responder :: Responder.Script r -- (6)
  }
```

The `method` field is a simple value, but the other fields point to what's called a *Script*. Scripts represent Okapi's DSL for extracting and parsing data from Requests. There's a specific type of Script for each part of a Request.

The type parameter of a Script represents the type of value it returns.

Therefore, the concrete type of an Endpoint is determined by the return types of
the Scripts that are used to construct the Endpoint.

All the different Script types are Applicatives, but not all are *lawful Applicatives*.

Since all Script types are Applicatives, we can use the Applicative typeclass methods to write our Scripts. Here's an example of a Query Script.

```haskell
data Filter = Filter
  { color :: Text
  , categoryID :: Int
  , isOnSale :: Maybe ()
  }

myQueryScript :: Query.Script Filter
myQueryScript = Filter
  <$> Query.param "color"
  <*> Query.param "category"
  <*> (Query.optional $ Query.flag "sale")
```

If you have the `-XApplicativeDo` language extension turned on, you can also write your Scripts using `do` syntax.

We recommend using `-XApplicativeDo` in conjuction with the `-XRecordWildCards` language extension if you're not comfortable with using the Applicative operators. Here's the same Query Script we defined above, but with these language extensions turned on.

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

myQueryScript :: Query.Script Filter
myQueryScript = do
  color      <- Query.param "color"
  categoryID <- Query.param "category"
  isOnSale   <- Query.optional $ Query.flag "sale"
  pure Filter {..}
```

Each Script type has its own operations suited to parsing its respective part of the
Request. These operations are covered in more detail below.

1. #### Method
   
   The `method` field represents the HTTP Method that the Endpoint accepts.
   
   Its type is `StdMethod` from the `http-types` library.
   
   Only the standard methods mentioned in RFC-1234 are allowed.

2. #### Path Script

   The `path` field defines the Request Path that the Endpoint accepts, including Path parameters.
   
   Path Scripts have two operations: `static` and `param`.

   ```haskell
   myPathScript = Path.static "person" *> Path.param @Int "personID"
   ```

3. #### Query Script

   The `query` field defines the Query that the Endpoint accepts.

   There are two operations for Query Scripts: `param` and `flag`.

   There are two modifiers for Query Scripts: `optional` and `option`.

   `optional` and `option` are specialized versions of the `optional` and `option`
   parser combinators found in the `parser-combinators` library.

   ```haskell
   myQueryScript :: Query.Script (Text, Maybe Float, Int)
   myQueryScript = do
     lastName  <- Query.param "last_name"
     optSalary <- Query.optional $ Query.param "salary"
     minAge    <- Query.option 21 $ Query.param "min_age"
     pure (lastName, optSalary, minAge)
   ```

4. #### Body Script

   The `body` field defines the Request Body that the Endpoint accepts.

   There are four operations for Body Scripts: `json`, `form`, `param`, `file`.

   There are two modifiers for Body Scripts: `optional` and `option`.

   ```haskell
   myBodyScript :: Body.Script (Maybe Value)
   myBodyScript = Body.optional $ Body.json @Value
   ```

5. #### Headers Script

   The `headers` field defines the Request Headers that the Endpoint accepts.

   There are two operations for Headers Scripts: `param` and `cookie`.

   There are two modifiers for Headers Scripts: `optional` and `option`.

   ```haskell
   myHeadersScript :: Headers.Script _
   myHeadersScript = undefined
   ```

6. #### Responder Script

   The `responder` field defines the Responses that the Endpoint's handler MUST return.

   Responder Scripts have to be more complex than the other Script types in order for the Endpoint to have a contract with its Handler. The contract ensures that the Handler will respond with the Responses defined in the Responder Script.

   This is done using a combination of higher order functions, linear types, and smart constructors.

   Responder Script operations have to take an *Add Header Script* as an argument to define what Headers will be attached to the Response.

   For now, there is only one operation for Responder Scripts: `json`.

   Add Header Scripts only have one operation as well: `using`.

   ```haskell
   {-# LANGUAGE ApplicativeDo #-}
   {-# LANGUAGE RecordWildCards #-}
   {-# LANGUAGE BlockArguments #-}
   {-# LANGUAGE LinearTypes #-}

   data SecretHeaders = SecretHeaders
     { firstSecret :: Int -> Response -> Response
     , secondSecret :: Int -> Response -> Response
     }

   data MyResponders = MyResponders
     { allGood :: (SecretHeaders %1 -> Response -> Response) -> Text -> Response
     , notGood :: (() %1 -> Response -> Response) -> Text -> Response
     }

   myResponderScript = do
     allGood <- Responder.json @Text status200 do
       addSecret <- AddHeader.using @Int "IntSecret"
       addAnotherSecret <- AddHeader.using @Int "X-Another-Secret"
       pure SecretHeaders {..}
     notGood <- Responder.json @Text status501 $ pure ()
     pure MyResponders {..}

   myHandler someNumber _ _ _ _ (MyResponders allGood notGood) = do
     if someNumber < 100
       then return $ allGood
         (\(SecretHeaders firstSecret secondSecret) response -> secondSecret 0 $ firstSecret 7 response)
         "All Good!"
       else return $ notGood
         (\() response -> response)
         "Not Good!"
   ```

   More information about *Responder* and *AddHeader* are available in the Handler section.

### Handler

Handlers are simple: they are contextful functions from the arguments provided by an Endpoint, to a Response.

The type synonym `Handler` represents the type of these contextful functions.

```haskell
type Handler m p q b h r = p -> q -> b -> h -> r -> m Response
```

The type parameter `m` represents the context in which the Handler creates the Response.

The type parameters `p`, `q`, `b`, `h` and `r` represent the types of the values returned by the Endpoint's Path, Query, Body, Headers and Responder Scripts respectively.

### Plan

A Plan is how your Endpoint and its designated Handler come together.

```haskell
data Plan m p q h b r = Plan
  { transformer :: m ~> IO
  , endpoint :: Endpoint p q h b r
  , handler :: Monad m => p -> q -> b -> h -> r -> m Response
  }
```

The `transformer` field represents a *natural transformation* from your Handler's Monad `m`, to `IO`. This is where you decide how you custom effects are interpreted in an `IO` context.

The `endpoint` field represents your Endpoint.

The `handler` field represents your Handler. The types must match the types of your `endpoint` and `transformer`.

Here's an example of a `Plan`:

```haskell
myPlan = Plan
  { transformer = id
  , endpoint = myEndpoint
  , handler = myHandler
  }

myEndpoint = Endpoint
  { method = GET
  , path = do
      Path.static "index"
      magicNumber <- Path.param @Int
      pure magicNumber
  , query = do
      x <- Query.param @Int "x"
      y <- Query.option 10 $ Query.param @Int "y"
      pure (x, y)
  , body = do
      foo <- Body.json @Value
      pure foo
  , headers = pure ()
  , responder = do
      itsOk <- Responder.json @Int HTTP.status200 do
        addSecretNumber <- AddHeader.using @Int "X-SECRET"
        pure addSecretNumber
      pure itsOk
  }

myHandler magicNumber (x, y) foo () responder = do
  let newNumber = magicNumber + x * y
  print newNumber
  print foo
  return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
```

The Server you build will be a combination of many Plans.

### Server

A Server is the final type of value that you need to generate an Application or Specification.

```haskell
data Server = Server
  { info :: Maybe Info
  , defaultResponse :: WAI.Response
  , artifacts :: [Artifact]
  }
```

The `info` field represents your Server's metadata. It's used in the generation of the Server's Specification. It's optional.

The `artifacts` field is a list of Artifact. A single Artifact is generated from a single Plan. An Artifact contains two values:

1. An IO action that returns a Response. It's only executed if the Endpoint used to generate the IO action matches the Request.
2. An OpenAPI PathItem value based on the structure of the Endpoint used to build the Artifact.

These two values, when combined with the Server's other Artifacts, are used to generate the final Application and OpenAPI Specification.

```haskell
build ::
  forall m p q h b r.
  Monad m =>
  Plan m p q h b r ->
  Artifact
build = ...

plan1 :: Plan A B C D E F
plan1 = ...

plan2 :: Plan G X Z U I P
plan2 = ...

plan3 :: Plan Y T L G N Q
plan3 = ...

myServer = Server
  { info = Nothing
  , defaultResponse = default404
  , artifacts =
      [ build plan1
      , build plan2
      , build plan3
      , ...
      ]
  }
```

The types of the Plans used to build your Server don't have to be the same. The `build` function erases the types and gives us the end products we need. This allows us to mix and match various combinations of Endpoints, Handlers, and Monad transformations in the same Server definition. For example, you can have two Handlers that operate in two different Monads in the same Server.

Now that you have you your Server, you can use it to:

1. Generate a WAI Application
2. Generate an OpenAPI Specification

```haskell
myServer :: Server
myServer = ...

api :: Application
api = genWaiApplication myServer

apiSpec :: OpenApi
apiSpec = genOpenAPISpec myServer
```

In the future, you should be able to automatically generate API clients as well.

### Tips & Ideas

#### Not Using A Plan

You can also create Servers with out first creating Plans. If you want to do this, you can just use the `buildWith` function directly.

```haskell
buildWith ::
  forall m p q h b r.
  Monad m =>
  (m ~> IO) ->
  Endpoint p q h b r ->
  Handler m p q h b r
  Artifact
buildWith transformer endpoint handler = ...
```

Assuming all of your handlers for the Server will run in the same context,
you can just partially apply `buildWith` to a transformation function and use the
partially applied function on your Endpoints and Handlers to produce Artifacts.

```haskell
buildWithIO = buildWith id

myServer = Server
  { info = Nothing
  , defaultResponse = default404
  , artifacts =
      [ buildWithIO endpoint1 handler1
      , buildWithIO endpoint2 handler2
      , buildWithIO endpoint3 \x y z a b -> do
          doSomethingWith x
          log a y b
          ...
      ]
  }
```

#### DRY Endpoints

When implementing an API you will usually need the same path to have multiple methods, each with different parameters in the query, body and headers. Since Endpoints are records, this is easy to deal with. Let's say we have a typical `/users/{userID : UserID}` route that accepts GET and PUT requests for fetching and updating a specific user respectively. The GET variant doesn't need a Body, but the PUT variant will.

```haskell
getUser = Endpoint
  { method = GET
  , path = do
      Path.static "users"
      userID <- Path.param @UserID
      pure userID
  , query = pure ()
  , body = pure () -- No Body Needed
  , headers = pure ()
  , responder = do
      ... -- The appropriate responses for a GET request
  }

putUser = getUser
  { method = PUT
  , body = Body.json @UpdatedUser -- Body Needed
  , responder = do
      ... -- The appropriate responses for a PUT request
  }
```

This way, we can define the `putUser` Endpoint by simply modifying `getUser` and without unnecessarily repeating our self.

## Matchpoint

A Matchpoint is a *pattern* that matches on Request values.

```haskell
pattern Matchpoint :: Request -> Matchpoint
```

You can use the Matchpoint pattern synonym to create your own pattern synonyms that match specific Requests.

```haskell
newtype UserID = UserID Int
  deriving ({- various typeclasses -})

pattern GetUserByID :: UserID -> Request
pattern GetUserByID userID <- Matchpoint
  GET
  ["users", PathParam @UserID userID]
  _
  _
  _
```

The `GetUserByID` pattern defined above would match against any Request of the form `GET /users/{userID : UserID}`. The Handler on the RHS of this pattern in a case statement will then be able to use the `userID` parameter in its function body if the Request matches sucessfully. If not, the next Matchpoint in your case statement is checked, just like regular patterns that we use all the time.

`PathParam` is a pattern synonym that you can use in your Matchpoints to match against path parameters of any type that are instances of both `ToHttpApiData` and `FromHttpApiData`. This is required since `PathParam` is a *bidirectional pattern synonym*. This property of `PathParam` makes it useful for generating URLs.

If your matching logic is more complicated, pattern synonyms alone may not be enough. For more complicated routes, we can use Okapi's DSL inside our Matchpoints by using `-XViewPatterns`. As an example, let's reimplement the first Endpoint on this page as a Matchpoint. Here's the Endpoint version first.

```haskell
-- | Define Endpoints using an Applicative eDSL
myEndpoint = Endpoint
  { method = GET
  , path = do
      Path.static "index"
      magicNumber <- Path.param @Int
      pure magicNumber
  , query = do
      x <- Query.param @Int "x"
      y <- Query.option 10 $ Query.param @Int "y"
      pure (x, y)
  , body = do
      foo <- Body.json @Value
      pure foo
  , headers = pure ()
  , responder = do
      itsOk <- Responder.json @Int status200 do
        addSecretNumber <- AddHeader.using @Int "X-SECRET"
        pure addSecretNumber
      pure itsOk
  }
```

Here's the equivalent Matchpoint version.

```haskell
-- | Define Matchpoints using the same DSL
pattern MyMatchpoint magicNumber pair foo = Matchpoint
  GET
  (Path.eval $ Path.static "index" *> Path.param @Int -> Ok magicNumber)
  (Query.eval xyQuery -> Ok pair)
  (Body.eval $ Body.json @Value -> Ok foo)
  _

xyQuery = do
  x <- Query.param @Int "x"
  y <- Query.option 10 $ Query.param @Int "y"
  pure (x, y)
```

We can simplify `MyMatchpoint` further by using more pattern synonyms.

```haskell
pattern MyMatchpoint n pair bar <- Matchpoint
  GET
  (MagicNumber n)
  (XYQuery pair)
  (Foo bar)
  _

pattern MagicNumber n <- (Path.eval $ Path.static "index" *> Path.param @Int -> Ok n)

pattern XYQuery pair <- (Query.eval xyQuery -> Ok pair)

pattern Foo baz <- (Body.eval $ Body.json @Value -> Ok baz)

xyQuery = do
  x <- Query.param @Int "x"
  y <- Query.option 10 $ Query.param @Int "y"
  pure (x, y)
```

Pattern synonyms like `MagicNumber` or `XYQuery` in the example above come in handy when we need to use the same pattern inside multiple Matchpoints.

You can use the Matchpoint we defined above in a case statement with other Matchpoints to define a Server.

```haskell
type Server m = Request -> m Response

myServer :: Server IO
myServer = \case
  MyMatchpoint n (x, y) foo -> do
    ...
  _ -> do
    ...

instantiate :: Monad m => (m ~> IO) -> Server m -> Application
instantiate transformer server = ...

api :: Application
api = instantiate id myServer
```

The Server type for Matchpoints is much simpler than the Server type for Endpoints.

### Matchpoints vs. Endpoints

We recommend using Endpoints. Matchpoints are great if you're not worried about safety and just want to get something up and running quickly. Here are some downsides to using Matchpoints to implement your Server:

1. We can't perform any static analysis on them. This means you can't generate OpenAPI specifications for Matchpoint Servers or perform any optimizations on them. You can perform static analysis on the Scripts that you use in your Matchpoints though, if there are any.

2. All Handlers in a Matchpoint Server must operate within the same context. For Endpoints, this is not the case.

3. Endpoints are more modular. You can achieve some level of modularity with your Matchpoints by using nested `-XPatternSynonyms` though.

4. Matchpoint Servers have no knowledge of what Responses you will return to the Client. Endpoint Servers know every possible Response you may return from your Handlers, besides the ones returned by `IO` errors (the goal is for Endpoints to know about these as well).

5. Requires knowledge of the `-XPatternSynonyms` and `-XViewPatterns` language extensions.

In short, if you don't care about safety, use Matchpoints.

## Servant <> Okapi

Coming Soon
