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

-- | Every definition from README.md, in one loadable module, so you can
--   poke at real types instead of reading them off the page.
--
--   Load it with:
--
--   > cabal repl lib:okapi
--   > :load lib/test/ReadmeExamples.hs
--   > :t calcServer
--   > :t myOpenApi
--
--   Deliberately excludes every @main@\/@Warp.run@ line from the README.
--   @warp@ isn't a dependency of the @okapi@ library itself, so those lines
--   can't compile against this package's own environment. Everything else
--   here is real, and this whole file is checked to load clean as one unit.
module ReadmeExamples where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Okapi
import Okapi.HTTP.Body qualified as Body
import Okapi.HTTP.Headers qualified as Headers
import Okapi.HTTP.Method qualified as Method
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Query qualified as Query
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Status qualified as Status
import Okapi.Link qualified as Link

-- * Hello World

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

-- * Calculator

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

-- * Generating Links (single route)

calcLink :: URI
calcLink = Link.build calcRequest (CalcArgs { operator = Add, x = 3, y = 4 }) []

-- * Generating an OpenAPI Document (single route)

printCalcOpenApi :: IO ()
printCalcOpenApi = LBS8.putStrLn (Aeson.encode (contractToOpenApi calcContract))

-- * Client (single route)

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

-- * Reverse

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

-- * Record-Based Servers

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

-- * Record-Based Clients

myClientSettings = do
    manager <- HC.newManager HC.defaultManagerSettings
    pure ClientSettings { manager, baseUrl = "http://localhost:8080" }

myClient = do
    settings <- myClientSettings
    pure (client contracts settings)

runRecordCalcClient :: IO ()
runRecordCalcClient = do
    routesClient <- myClient
    result <- routesClient.calc.fetch calcRequestValue
    case result of
        Left ClientError -> putStrLn "client error"
        Right (Success resData)   -> resData.body >>= print
        Right (DivByZero resData) -> resData.body >>= print

-- * Record-Based Links

myLinks = links contracts

calcURI = myLinks.calc.build (CalcArgs { operator = Add, x = 3, y = 4 }) []

-- * Record-Based OpenAPI (all routes)

myOpenApi = openApi contracts

printMyOpenApi :: IO ()
printMyOpenApi = LBS8.putStrLn (Aeson.encode myOpenApi)

-- * Transformer: Per-Endpoint Monadic Contexts

-- | A minimal, hand-rolled Reader, using nothing beyond @base@.
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
