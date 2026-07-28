# 🦓🦒Okapi

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
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Network.Wai.Handler.Warp qualified as Warp

helloRequest = Req.get & Req.path (Req.lit "hello")

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
import Network.HTTP.Types qualified as Types
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
    = Success (f Status.S200 Types.ResponseHeaders (IO Integer))
    | DivByZero (f Status.S422 Types.ResponseHeaders (IO Text))
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
