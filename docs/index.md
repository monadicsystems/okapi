# 🦓🦒Okapi

Okapi is a micro framework for implementing HTTP servers.

- Ergonomic DSL for parsing requests
- Integrates with ANY monad stack or effect system
- Automagically generate OpenAPI specifications (clients coming soon)

## Getting Started

1. Use the command `stack new <project-name>` to create a new Haskell project
2. Add the `okapi` library to your project's dependencies

## Introduction

There are two ways to implement servers in Okapi.

The recommended way to implement a server in Okapi is via *Endpoints*:

```haskell
-- | Define Endpoints using an Applicative eDSL
myEndpoint = Endpoint
  GET
  do
    Path.static "index"
    magicNumber <- Path.param @Int
    pure magicNumber
  do
    x <- Query.param @Int "x"
    y <- Query.option 10 $ Query.param @Int "y"
    pure (x, y)
  do
    foo <- Body.json @Value
    pure foo
  do pure ()
  do
    itsOk <- Responder.json @Int status200 do
      addSecretNumber <- AddHeader.using @Int "X-SECRET"
      pure addSecretNumber
    pure itsOk
```

An alternative, more concise way of defining a server in Okapi is via *Matchpoints*:

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
automatically generate specifications and clients for a server implemented
with Endpoints, but not a server implemented with Matchpoints.

On the flip side, a server implemented with Matchpoints will be more
concise than the same server implemented with Endpoints.

## Endpoint

An *Endpoint* is an *executable specification* that represents a single endpoint of
your API.

An Endpoint has 6 fields:

```haskell
data Endpoint p q h b r = Endpoint
  { method :: StdMethod,                  -- (1)
    pathScript :: Path.Script p,          -- (2)
    queryScript :: Query.Script q,        -- (3)
    headersScript :: Headers.Script h,    -- (4)
    bodyScript :: Body.Script b,          -- (5)
    responderScript :: Responder.Script r -- (6)
  }
```

The `method` field is a simple value, but the other fields point to a `Script` that represents their respective HTTP request parts.

The type parameter of a Script represents the type of value it returns.

Therefore, the concrete type of an Endpoint is determined by the return types of
the Scripts that are used to construct the Endpoint.

All the different Script types are Applicatives, but not all are *lawful applicatives*.

Since all Script types are Applicatives, we can use the Applicative typeclass methods to write our scripts. Here's an example of a query Script:

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

We recommend using `-XApplicativeDo` in conjuction with the `-XRecordWildCards` language extension if you're not comfortable with using the Applicative operators. Here's the same query script we defined above, but with these language extensions turned on:

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

Each Script type has its' own operations suited to parsing its' respective part of the
request. These are defined in more detail below.

1. #### Method
   
   The `method` field represents the HTTP method that the Endpoint accepts.
   
   Its' type is `StdMethod` from the `http-types` library.
   
   Only the standard methods mentioned in RFC-1234 are allowed.

2. #### Path Script

   The `pathScript` field defines the request path that the Endpoint accepts, including *path parameters*.
   
   Path Scripts have two operations: `static` and `param`.

   ```haskell
   myPathScript = Path.static "person" *> Path.param @Int "personID"
   ```

3. #### Query Script

   The `queryScript` field defines the query that the Endpoint accepts.

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

   The `bodyScript` field defines the request body and it's content type that the Endpoint accepts.

   There are four operations for Body Scripts: `json`, `form`, `formParam`, `file`.

   There are two modifiers for Body Scripts: `optional` and `option`.

   ```haskell
   myBodyScript :: Body.Script (Maybe Value)
   myBodyScript = Body.optional $ Body.json @Value
   ```

5. #### Headers Script

   The `headersScript` field defines the request headers that the Endpoint accepts.

   There are two operations for Headers Scripts: `param` and `cookie`.

   There are two modifiers for Headers Scripts: `optional` and `option`.

   ```haskell
   myHeadersScript :: Headers.Script _
   myHeadersScript = undefined
   ```

6. #### Responder Script

   The `responderScript` field defines the responses that the Endpoint's handler MUST return.

   Responder Scripts have to be more complex than the other Script types in order for the Endpoint to have a contract with the handler that the handler will respond with the responses defined in the Responder Script.

   This is done using a combination of higher order functions, linear types, and smart constructors.

   Responder Script operations have to take a *Add Header* Script as an argument to define what headers will be attached to the Response.

   For now, there is only one operation for Responder Scripts: `json`.

   Add Header Scripts also only have one operation: `has`.

   ```haskell
   {-# LANGUAGE ApplicativeDo #-}
   {-# LANGUAGE RecordWildCards #-}
   {-# LANGUAGE BlockArguments #-}

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

   More information about *Responders* and *AddHeader* is available in the Handler section.

### Handler

Handlers are simple: they are contextful functions from the arguments provided by an Endpoint, to a Response.

The type synonym `Handler` represents the type of Handlers in Okapi:

```haskell
type Handler m p q b h r = p -> q -> b -> h -> r -> m Response
```

The type parameter `m` represents the context in which the Handler creates the Response.

The type parameters `p`, `q`, `b`, `h` and `r` represent the types of the values returned by the Endpoint's Path, Query, Body, Headers and Responder Scripts respectively.

### Plan

A Plan is how your Endpoint and a compatible Handler come together.

```haskell
data Plan m p q h b r = Plan
  { transformer :: m ~> IO,
    endpoint :: Endpoint p q h b r,
    handler :: Monad m => p -> q -> b -> h -> r -> m Response
  }
```

The `transformer` field represents a natural transformation from your handler's Monad `m` to `IO`. This is where you handle how you're custom effects are interpreted in `IO`.

The `endpoint` field represents your Endpoint.

The `handler` field represents your Handler. The types must match the types of your `endpoint` and `transformer`.

Here's an example of a `Plan`:

```haskell
myPlan = Plan
  id
  myEndpoint
  myHandler

myEndpoint = Endpoint
  GET
  do
    Path.static "index"
    magicNumber <- Path.param @Int
    pure magicNumber
  do
    x <- Query.param @Int "x"
    y <- Query.option 10 $ Query.param @Int "y"
    pure (x, y)
  do
    foo <- Body.json @Value
    pure foo
  do pure ()
  do
    itsOk <- Responder.json @Int HTTP.status200 do
      addSecretNumber <- AddHeader.using @Int "X-SECRET"
      pure addSecretNumber
    pure itsOk

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
  { info :: Maybe Info,
    defaultResponse :: WAI.Response,
    artifacts :: [Artifact]
  }
```

The `info` field represents your Server's metadata. It's used in the generation of the Server's specification. It's optional.

The `artifacts` field is a list of Artifact. A single Artifact is generated from a single Plan, which contains a `transformer`, an Endpoint and a Handler. An Artifact contains two values that are generated from a Plan:

1. An IO action that returns a Response. It's only executed if the Endpoint used to generate the IO action matches the Request.
2. An OpenAPI PathItem value based on the structure of the Endpoint used to build the Artifact.

These two values, when combined with the Server's other Artifacts, are used to generate the final Application and OpenAPI Specification respectively.

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
  Nothing
  default404
  [ build plan1
  , build plan2
  , build plan3
  , ...
  ]
```

Notice the types of the Plans don't have to be the same. The build function erases the types and gives us the end product that we want. This allows you to mix and match various combinations of Endpoints, Handlers, and Monad transformations in the same Server definition. For example, you can have two handlers that run in two different Monads in the same Server.

Now that you have you your Server you can either:

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

## Matchpoint

## Servant <> Okapi

<!-- ## TLDR

or even

```haskell
{-# LANGUAGE ApplicativeComprehensions #-}

myEndpoint' = Endpoint
  GET
  [ n | Path.static "index", n <- Path.param @Int ]
  [ (x, y) | x <- Query.param @Int "x", y <- Query.option 10 $ Query.param @Int "y" ]
  [ foo | foo <- Body.json @Value ]
  [ () | ]
  [ itsOk |
    itsOk <- Responder.json
      @Int
      status200
      [ addSecretNumber | addSecretNumber <- AddHeader.using @Int "X-SECRET" ]
  ]
```

this works when `-XApplicativeComprehensions` is turned on. -->
