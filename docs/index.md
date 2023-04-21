# 🦓🦒Okapi

## Introduction

Okapi is a micro framework for implementing HTTP servers.

- Ergonomic DSL for routing and parsing requests
- Integrate Okapi with ANY monad stack or effect system
- Automatically generate clients and OpenAPI specifications

The primary way to implement a server in Okapi is via *Endpoints*:

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
    itsOk <- Responder.json @Int HTTP.status200 do
      addSecretNumber <- ResponderHeaders.has @Int "X-SECRET"
      pure addSecretNumber
    pure itsOk

-- | Define Handlers for Endpoints
myHandler magicNumber (x, y) foo () responder = do
  let newNumber = magicNumber + x * y
  print newNumber
  print foo
  return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber

-- | Combine Endpoints with Handlers to create Plans
myPlan = Plan
  myEndpoint
  myHandler

-- | Compose Plans with other Plans to create Servers
myServer = Server
  Nothing
  default404
  [ myPlan
  , Plan myOtherEndpoint myOtherHandler
  , anotherPlan
  ]

-- | Run your Servers using Warp
main = Warp.run 3000 $ instantiate id myServer
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
concise than a server implemented with Endpoints.

## Getting Started

1. Use the command `stack new <project-name>` to create a new Haskell project
2. Add the `okapi` library to your project's dependencies

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

   Responder Script operations have to take a *Responder Headers* Script as an argument to define what headers will be attached to the Response.

   For now, there is only one operation for Responder Scripts: `json`.

   Responder Header Scripts also only have one operation: `has`.

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
       addSecret <- ResponderHeaders.has @Int "IntSecret"
       addAnotherSecret <- ResponderHeaders.has @Int "X-Another-Secret"
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

   More information about *Responders* and *ResponderHeaders* is available in the Handler section.

### Handler

Handlers are simple: they are contextful functions from the arguments provided by an Endpoint, to a Response.

The type synonym `Handler` represents the type of Handlers in Okapi:

```haskell
type Handler m p q b h r = p -> q -> b -> h -> r -> m Response
```

The type parameter `m` represents the context in which the Handler creates the Response.

The type parameters `p`, `q`, `b`, `h` and `r` represent the types of the values returned by the Endpoint's Path, Query, Body, Headers and Responder Scripts respectively.

### Plan

A Plan

### Server

### Lifter

### Execution

## Matchpoint

## TLDR
