# 🦓🦒Okapi

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
      addSecretNumber <- AddHeader.using @Int "X-SECRET"
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

-- | Servers are just contextful functions from a Matchpoint to a Response
type Server m = Matchpoint -> m Response

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
