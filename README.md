# Okapi

Okapi is a bidirectional DSL for describing HTTP servers.

- Ergonomic DSLs for parsing and printing requests and responses
- Integrate Okapi with ANY monad stack or effect system
- Generate clients and OpenAPI specifications for free
- All in less than 5000 lines of code

## Hello World Example

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Okapi
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Network.Wai.Handler.Warp qualified as Warp

helloRequest = Req.get & Req.path (Path.lit "hello")

helloResponse = Res.ok

helloContract = helloRequest :-> helloResponse

helloFunction = fn \(_req, _raw) ->
    return Res.Data
        { status = 200
        , headers = []
        , body = pure "Hello, World!"
        }

helloServer = server id id helloContract helloFunction

main :: IO ()
main = Warp.run 8080 (route helloServer catchAll)
```

## Calculator Example

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp qualified as Warp
import Okapi
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Status qualified as Status

data Operator = Add | Sub | Mul | Div deriving (Show, Eq)

-- | A path segment leaf for "add"/"sub"/"mul"/"div".
operatorLeaf = Leaf
    { decode = \t -> case t of
        "add" -> Right Add
        "sub" -> Right Sub
        "mul" -> Right Mul
        "div" -> Right Div
        _     -> Left Path.ParseError
    , encode = \op -> case op of
        Add -> "add"
        Sub -> "sub"
        Mul -> "mul"
        Div -> "div"
    , info = Info "operator" Nothing
    }

data CalcArgs = CalcArgs
    { operator :: Operator
    , x        :: Integer
    , y        :: Integer
    }

-- | Matches /calc/{operator}/{x}/{y}, e.g. /calc/add/3/4.
calcPath = do
    Path.lit "calc"
    operator <- (.operator) =. Path.seg "operator" operatorLeaf
    x        <- (.x)        =. Path.seg "x" integer
    y        <- (.y)        =. Path.seg "y" integer
    pure CalcArgs { operator, x, y }

calcRequest = Req.get & Req.path calcPath

data CalcResponses f
    = Success (f Status.S200 Headers.Base (IO Integer))
    | DivByZero (f Status.S422 Headers.Base (IO Text))
    deriving (Generic, Responses)

successResponse = Res.ok & Res.body (Body.json @Integer)

divByZeroResponse = Res.unprocessableEntity & Res.body (Body.json @Text)

calcResponses = responses @CalcResponses successResponse divByZeroResponse

calcContract = calcRequest :-< calcResponses

calcFunction = fn \(req, _raw) ->
    let args = req.path
        okResult n    = Res.Data { status = 200, headers = [], body = pure n }
        errResult msg = Res.Data { status = 422, headers = [], body = pure msg }
    in return case args.operator of
        Add -> Success (okResult (args.x + args.y))
        Sub -> Success (okResult (args.x - args.y))
        Mul -> Success (okResult (args.x * args.y))
        Div
          | args.y == 0 -> DivByZero (errResult "Cannot divide by zero")
          | otherwise   -> Success (okResult (args.x `div` args.y))

calcServer = server id id calcContract calcFunction

main :: IO ()
main = Warp.run 8080 (route calcServer catchAll)
```

## Composing Routes

Both examples above run standalone, each with their own `main`. `route` produces an
ordinary `Wai.Middleware`, so multiple routes compose with plain `(.)`,
chained down onto one final fallback (`catchAll`, or your own):

```haskell
main :: IO ()
main =
    Warp.run 8080
        $ route helloServer
        . route calcServer
        $ catchAll
```

Assuming `helloServer` and `calcServer` from the two examples above are
both in scope, this serves `GET /hello` and `GET /calc/{operator}/{x}/{y}`
from the same application. The first `route` whose contract matches the
incoming request handles it; anything neither recognizes falls through to
`catchAll`.

## Generating Links

A `Contract`'s request codec already knows how to print a path and query, and
`Okapi.Link.build` reuses exactly that to turn a concrete path/query value
into a `URI`, without touching a server or the network:

```haskell
import Okapi.Link qualified as Link

calcLink :: URI
calcLink = Link.build calcRequest (CalcArgs { operator = Add, x = 3, y = 4 }) []
```

`calcLink.full` is `"/calc/add/3/4"`. Assuming `calcRequest`/`CalcArgs` from
the Calculator example above are in scope, this stays correct for free as
`calcPath` changes, since there's no separate URL template to keep in sync by
hand.

## Generating an OpenAPI Document (Single Route)

`contractToOpenApi` turns any `Contract` into a `Data.OpenApi.OpenApi`
document, the same way `Ex1.hs` in this repo does for a real endpoint:

```haskell
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8

printCalcOpenApi :: IO ()
printCalcOpenApi = LBS8.putStrLn (Aeson.encode (contractToOpenApi calcContract))
```

This prints a real OpenAPI document generated straight from `calcContract`,
including the `operator`/`x`/`y` path parameters and both the `200` and `422`
responses from `CalcResponses`, all derived from the same contract the
server itself runs:

```json
{
  "openapi": "3.0.0",
  "info": { "title": "API", "version": "0.1.0" },
  "components": {},
  "paths": {
    "/calc/{operator}/{x}/{y}": {
      "get": {
        "parameters": [
          { "in": "path", "name": "operator", "required": true, "schema": { "type": "string" } },
          { "in": "path", "name": "x", "required": true, "schema": { "type": "integer" } },
          { "in": "path", "name": "y", "required": true, "schema": { "type": "integer" } }
        ],
        "responses": {
          "200": { "description": "200", "content": { "application/json": { "schema": { "type": "integer" } } } },
          "422": { "description": "422", "content": { "application/json": { "schema": { "type": "string" } } } }
        }
      }
    }
  }
}
```

## Client

`clientFor` builds a callable client function directly from a `Contract`,
with no separate client-generation step. `Client` has a `.fetch` field (via
`OverloadedRecordDot`) that reaches straight into it as a plain function from
a `Req.Data` to `IO (Either ClientError result)`, no pattern match needed:

```haskell
import Network.HTTP.Client qualified as HC
import Okapi.HTTP.Method qualified as Method
import Okapi.HTTP.Query qualified as Query

calcClientSettings :: IO ClientSettings
calcClientSettings = do
    manager <- HC.newManager HC.defaultManagerSettings
    pure ClientSettings { manager, baseUrl = "http://localhost:8080" }

calcRequestValue :: Req.Data Method.GET CalcArgs Query.Base Headers.Base Body.Base
calcRequestValue = Req.Data
    { method = Method.Get
    , path = CalcArgs { operator = Add, x = 3, y = 4 }
    , query = []
    , headers = []
    , body = pure mempty
    }

runCalcClient :: IO ()
runCalcClient = do
    settings <- calcClientSettings
    result <- (clientFor settings calcContract).fetch calcRequestValue
    case result of
        Left ClientError -> putStrLn "client error"
        Right (Success resData)   -> resData.body >>= print
        Right (DivByZero resData) -> resData.body >>= print
```

`calcContract` has multiple response shapes, so `result` is
`Either ClientError (CalcResponses Res.Data)`. Pattern match on it exactly
the way `calcFunction` builds it on the server side, `Success`/`DivByZero`
and all. Assuming `calcServer` from the Calculator example above is running,
`runCalcClient` prints `7`.

`.fetch` is purely additive; `let Fn calculate = clientFor settings
calcContract` still works too, for anywhere pattern-matching `Client`
directly is more convenient than a field access.

## Reverse Example

A third route, in the same style as the first two: `GET /reverse/{word}`,
one path segment, one response.

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wai.Handler.Warp qualified as Warp
import Okapi
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res

reversePath = do
    Path.lit "reverse"
    word <- Path.seg "word" text
    pure word

reverseRequest = Req.get & Req.path reversePath

reverseResponse = Res.ok & Res.body (Body.json @Text)

reverseContract = reverseRequest :-> reverseResponse

reverseFunction = fn \(req, _raw) ->
    return Res.Data { status = 200, headers = [], body = pure (T.reverse req.path) }

reverseServer = server id id reverseContract reverseFunction

main :: IO ()
main = Warp.run 8080 (route reverseServer catchAll)
```

Now there are three routes in scope: `helloContract`, `calcContract`, and
`reverseContract`. The sections below build a `Server`, a `Client`, a set of
`Link`s, and an OpenAPI document for all three at once, from one shared
record.

## Record-Based Servers

Naming and combining routes by hand is fine for a couple of them, but it
gets old past a handful, and there's a better reason to reach for a record
than just saving keystrokes (the sections after this one). `Base`, the
all-caps field tags (`METHOD`/`PATH`/`RESPONSES`/...), and `(:&)` from
`Okapi.Contract` exist for exactly this: writing a route's full six-argument
`Signature` by hand is a lot of ceremony, so instead start from `Base` (the
fully unconstrained `Signature`) and override only the slots that actually
differ:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

import GHC.Generics (Generic)
import Okapi.HTTP.Method qualified as Method

type HelloShape =
    Base
        :& METHOD Method.GET
        :& PATH ()
        :& RESPONSES (Res.Data Status.S200 Headers.Base Body.Base)

type CalcShape =
    Base
        :& METHOD Method.GET
        :& PATH CalcArgs
        :& RESPONSES (CalcResponses Res.Data)

type ReverseShape =
    Base
        :& METHOD Method.GET
        :& PATH Text
        :& RESPONSES (Res.Data Status.S200 Headers.Base (IO Text))

data Routes f = Routes
    { hello   :: f HelloShape
    , calc    :: f CalcShape
    , reverse :: f ReverseShape
    } deriving (Generic)

contracts = Routes { hello = helloContract, calc = calcContract, reverse = reverseContract }

handlers = Routes { hello = helloFunction, calc = calcFunction, reverse = reverseFunction }

myServers = servers id contracts handlers

main :: IO ()
main = Warp.run 8080 (mountAll (handles myServers) catchAll)
```

`servers` builds every field's `Server` from one shared record of
`Contract`s and one of `Function`s, sharing one natural transformation
(`id`, since every route here is already plain `IO`). `handles` collapses
the resulting `Routes (Server IO)` down to the foundational `[Handle]`
representation, and `mountAll` folds that into a `Wai.Middleware` the same
way `route`/`(.)` did earlier, just for a whole record at once instead of
one route at a time.

`contracts` is the value the next three sections reuse.

## Record-Based Clients

The same `contracts` value that fed `servers` above is also everything a
client needs, since a `Contract` alone already describes how to print a
request and parse a response:

```haskell
import Network.HTTP.Client qualified as HC

myClientSettings = do
    manager <- HC.newManager HC.defaultManagerSettings
    pure ClientSettings { manager, baseUrl = "http://localhost:8080" }

myClient = do
    settings <- myClientSettings
    pure (client contracts settings)
```

`myClient` is `IO (Routes Client)`. Running it produces a `Routes Client`
whose `.calc` field is a real, directly-typed `Client` value — chain
straight through to `.fetch`, the same field access as the single-route
Client example above:

```haskell
routesClient <- myClient
result <- routesClient.calc.fetch calcRequestValue
```

There's no separate client-generation step; the client comes straight out of
the same `contracts` the server runs.

## Record-Based Links

Same idea again, this time for URLs:

```haskell
myLinks = links contracts

calcURI = myLinks.calc.build (CalcArgs { operator = Add, x = 3, y = 4 }) []
```

`calcURI.full` is `"/calc/add/3/4"`, same as the single-route `calcLink`
earlier, since `links` just builds one `Link` per field at once. `Link` has
a `.build` field the same way `Client` has `.fetch`, reaching straight into
the wrapped `path -> query -> URI` function without pattern-matching its
`Builder` constructor.

## Record-Based OpenAPI (All Routes)

And for documentation, `openApi` merges every field's OpenAPI document into
one, via `OpenApi`'s own `Monoid` instance:

```haskell
myOpenApi = openApi contracts
```

Encoding `myOpenApi` with Aeson (the same way `printCalcOpenApi` did
earlier) produces one document covering all three routes:

<details>
<summary>Generated OpenAPI document</summary>

```json
{
  "openapi": "3.0.0",
  "info": { "title": "API", "version": "0.1.0" },
  "components": {},
  "paths": {
    "/hello": {
      "get": {
        "responses": { "200": { "description": "200" } }
      }
    },
    "/calc/{operator}/{x}/{y}": {
      "get": {
        "parameters": [
          { "in": "path", "name": "operator", "required": true, "schema": { "type": "string" } },
          { "in": "path", "name": "x", "required": true, "schema": { "type": "integer" } },
          { "in": "path", "name": "y", "required": true, "schema": { "type": "integer" } }
        ],
        "responses": {
          "200": { "description": "200", "content": { "application/json": { "schema": { "type": "integer" } } } },
          "422": { "description": "422", "content": { "application/json": { "schema": { "type": "string" } } } }
        }
      }
    },
    "/reverse/{word}": {
      "get": {
        "parameters": [
          { "in": "path", "name": "word", "required": true, "schema": { "type": "string" } }
        ],
        "responses": {
          "200": { "description": "200", "content": { "application/json": { "schema": { "type": "string" } } } }
        }
      }
    }
  }
}
```

</details>

Three routes, one `contracts` value, one merged document, no separate step
to keep it in sync with what the server actually serves.

## Transformer: Per-Endpoint Monadic Contexts

Everything above shares one monad, `IO`, across the whole record. Sometimes
routes genuinely don't share one: some run in plain `IO`, others in an
application monad carrying config, a database connection, or anything else.
`serversVia` is the heterogeneous counterpart to `servers`, and each field
supplies its own monad instead of one shared one, via `Transformer` (a
natural transformation down to `IO`) and `Morph` (which lifts a plain
`Contract` into the two-argument slot a heterogeneous record needs).

This needs a route that actually wants something other than `IO` to make
the point, so here's a small one built just for this section: `GET
/greet/{name}`, whose handler reads a greeting prefix out of its own
environment instead of hard-coding one. `AppM` below is a minimal,
hand-rolled Reader, using nothing beyond `base`:

```haskell
newtype AppM a = AppM (Text -> IO a)

instance Functor AppM where
    fmap f (AppM g) = AppM (fmap f . g)

instance Applicative AppM where
    pure x = AppM (\_ -> pure x)
    AppM f <*> AppM x = AppM (\r -> f r <*> x r)

instance Monad AppM where
    AppM x >>= f = AppM (\r -> x r >>= \a -> case f a of AppM g -> g r)

ask :: AppM Text
ask = AppM pure

greetPath = do
    Path.lit "greet"
    name <- Path.seg "name" text
    pure name

greetRequest = Req.get & Req.path greetPath

greetResponse = Res.ok & Res.body (Body.json @Text)

greetContract = greetRequest :-> greetResponse

greetFunction = fn \(req, _raw) -> do
    prefix <- ask
    return Res.Data { status = 200, headers = [], body = pure (prefix <> ", " <> req.path <> "!") }

type GreetShape =
    Base
        :& METHOD Method.GET
        :& PATH Text
        :& RESPONSES (Res.Data Status.S200 Headers.Base (IO Text))

data RoutesVia f = RoutesVia
    { hello :: f IO HelloShape
    , greet :: f AppM GreetShape
    } deriving (Generic)

transforms = RoutesVia
    { hello = Transformer id
    , greet = Transformer (\(AppM act) -> act "Hello")
    }

contractsVia = RoutesVia
    { hello = morph helloContract
    , greet = morph greetContract
    }

handlersVia = RoutesVia
    { hello = helloFunction
    , greet = greetFunction
    }

myServersVia = serversVia transforms contractsVia handlersVia

main :: IO ()
main = Warp.run 8080 (mountAll (handles myServersVia) catchAll)
```

`hello` stays plain `IO`; `greet` runs in `AppM`, and its `Transformer`
(`\(AppM act) -> act "Hello"`) is what actually supplies the "Hello" prefix
at the boundary where a request gets handled. `RoutesVia`'s fields each
carry their own monad (`f IO HelloShape` vs. `f AppM GreetShape`). A
record whose field type takes one type argument, like `Routes` above,
can't let fields disagree on a monad; giving each field two arguments to
work with is what makes this a genuinely different capability, not just a
variant spelling of `servers`. Hitting `GET /greet/Ada` against this server
returns `"Hello, Ada!"`.
