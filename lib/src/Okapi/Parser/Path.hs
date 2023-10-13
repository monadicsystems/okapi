{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Parser.Path where

import Control.Natural
import Data.Data (Typeable)
import Data.Function ((&))
import Data.Kind
import Data.Map
import Data.Text
import Data.Typeable
import GHC.Base (undefined)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.NewDSL
import Okapi.Route qualified as Route
import Web.HttpApiData qualified as Web

data Expr a where
  Static :: Web.ToHttpApiData a => a -> Expr ()
  Param :: Web.FromHttpApiData a => Expr a
  --   Optional :: Expr a -> Expr (Maybe a)
  --   Macro :: Context Expr state error => DSL Expr state error a -> Expr a
  End :: Expr ()

type State = [Text]

data Error where
  Error :: Text -> Error

instance Context Expr State Error where
  eval state expr = case expr of
    Static @t x -> undefined
    Param @t -> undefined
    -- Optional expr' -> undefined
    -- Macro dsl -> undefined
    End -> undefined

-- embed :: Expr a -> DSL Expr State Error a
-- embed = Eval interpreter

static :: Web.ToHttpApiData a => a -> DSL Expr State Error ()
static = Expr . Static

param :: Web.FromHttpApiData a => DSL Expr State Error a
param = Expr Param

-- optional :: Expr a -> DSL Expr State Error (Maybe a)
-- optional = Expr . Optional

end :: DSL Expr State Error ()
end = Expr End

-- instance DSL Expr [Text] Error where
--   eval :: Expr -> [Text] -> (Either Error Result, [Text])
--   eval (Static @t x) input = (Right $ StaticResult (), [])
--   eval (Param @t) input = undefined
--   eval End [] = (Right $ EndResult (), [])

-- Compile Time - while code is compiling and typechecking. Type level errors occur here
-- Run Time - code is running. It has typechecked, but runtime errors like IO errors can still occur.
-- App Time - the Okapi app is running. Before running the app, certain errors can be caught

class FromPath a where
  -- pattern Path :: a -> [Text]
  route :: DSL Expr State Error a
  {-# MINIMAL route #-}

fromPath :: FromPath a => [Text] -> Either Error a
fromPath path = fst (exec path route)

class ToPath a where
  toPath :: a -> [Text]
  {-# MINIMAL toPath #-}

class (Eq a, FromPath a, ToPath a) => Route a where
  roundtrip :: a -> Bool
  roundtrip x = case fromPath (toPath x) of
    Left _ -> False
    Right x' -> x == x'

-- instance (Eq a, Typeable a) => Eq (DSL Expr State Error a) where
{-
(^==) :: DSL Expr State Error a -> DSL Expr State Error b -> Bool
FMap f dsl ^== FMap f' dsl' = dsl ^== dsl'
Pure x ^== Pure x' = Just x == cast x'
Apply dslF dslX ^== Apply dslF' dslX' = dslX ^== dslX'
Expr expr ^== Expr expr' = case (expr, expr') of
  (Static s, Static s') -> Just s == cast s'
  (Param @p, Param @p') -> convert @p == convert @p'
  (End, End) -> True
  (_, _) -> False
_ ^== _ = False
-}

data API where
  Router :: forall a env. (env ~> IO) -> DSL Expr State Error a -> (Wai.Request -> a -> env Wai.Response) -> API
  Endpoint :: forall a env. (env ~> IO) -> (HTTP.StdMethod -> Bool) -> DSL Expr State Error a -> (Wai.Request -> a -> env Wai.Response) -> API
  MethodMap :: forall a env. (env ~> IO) -> DSL Expr State Error a -> Map HTTP.StdMethod (Wai.Request -> a -> env Wai.Response) -> API
  Scope :: [Text] -> Wai.Middleware -> [API] -> API

-- ?? Scope :: [Text] -> ((Wai.Request -> env Wai.Response) -> Wai.Request -> env Wai.Response) -> [API] -> API

router :: forall a env. (env ~> IO) -> DSL Expr State Error a -> (Wai.Request -> a -> env Wai.Response) -> API
router = Router

endpoint :: forall a env. (env ~> IO) -> (HTTP.StdMethod -> Bool) -> DSL Expr State Error a -> (Wai.Request -> a -> env Wai.Response) -> API
endpoint = Endpoint @a @env

methodMap :: forall a env. (env ~> IO) -> DSL Expr State Error a -> Map HTTP.StdMethod (Wai.Request -> a -> env Wai.Response) -> API
methodMap = MethodMap @a @env

scope :: [Text] -> Wai.Middleware -> [API] -> API
scope = Scope

myAPI :: API
myAPI = router id (route @Person) \req -> \Person {..} -> do
  undefined

myAPI' :: API
myAPI' = endpoint id (HTTP.GET==) (route @Person) \req Person {..} -> do
  undefined

myAPI'' :: API
myAPI'' = endpoint id (`Prelude.elem` [HTTP.GET, HTTP.HEAD, HTTP.POST]) (route @Person) \req Person {..} -> do
  undefined

myAPI''' :: API
myAPI''' = endpoint id (const True) (route @Person) \req Person {..} -> do
  undefined

(|>) = (&)

anotherAPI :: API
anotherAPI =
  methodMap id (route @Person) $
    Data.Map.empty
      |> insert HTTP.GET (\req Person {..} -> do undefined)
      |> insert HTTP.PUT (\req Person {..} -> do undefined)

fullAPI :: API
fullAPI =
  scope
    ["hello", "world"]
    id
    [ myAPI,
      myAPI',
      anotherAPI
    ]

helloWorld :: Route.Route (Text, Int)
helloWorld = do
  _ <- Route.static "hello"
  _ <- Route.static "world"
  name <- Route.param @Text
  age <- Route.param @Int
  pure (name, age)

helloWorld' :: Route.Route (Text, Int)
helloWorld' = do
  _ <- Route.static "helloz"
  _ <- Route.static "world"
  name <- Route.param @Text
  age <- Route.param @Int
  pure (name, age)

helloWorld'' :: Route.Route (Text, Int)
helloWorld'' = do
  _ <- Route.static "hello"
  _ <- Route.static "world"
  age <- Route.param @Int
  name <- Route.param @Text
  pure (name, age)

data Person = Person {name :: Text, age :: Int, salary :: Float}

-- subRoute :: Route.Route Person
-- -- subRoute :: Route.Route (Text, Int) -> Route.Route Person
-- subRoute = do
--   salary <- Route.param @Float
--   (name, age) <- helloWorld
--   pure Person {..}

{-
  do
    (name, age) <- helloW
    float <- Route.param @Float
    pure (name, age, float)
-}

-- bigRoute :: Route.Route ((Text, Int), (Text, Int))
-- bigRoute = do
--   -- (name, age) <- helloWorld
--   (name', age') <- helloWorld'
--   pure ((name, age), (name', age'))

xRoute :: Route.Route Int
xRoute = do
  _ <- Route.static "x"
  x <- Route.param @Int
  pure x

yRoute :: Route.Route Int
yRoute = do
  _ <- Route.static "y"
  y <- Route.param @Int
  pure y

zRoute :: Route.Route Int
zRoute = do
  _ <- Route.static "z"
  x <- xRoute
  y <- yRoute
  pure (x + y)

xyRoute :: Route.Route (Int, Int)
xyRoute = do
  _ <- Route.static "xy"
  x <- xRoute
  y <- yRoute
  pure (x, y)

data Datum = Datum {foo :: Int, bar :: Int, baz :: Int}

-- xyzRoute :: Route.Route (Text, Int) -> Route.Route Datum
-- xyzRoute alphaR = do
--   _ <- Route.static "alphay"
--   (_, foo) <- alphaR
--   (bar, baz) <- xyRoute
--   pure Datum {..}

-- resource :: forall a env. Route a => (env ~> IO) -> API
-- resource t = (Resource @a @env) { transform = t, gets = Nothing, posts = Nothing, puts = Nothing }
-- makeEndpoints :: forall a env. Route a => (env ~> IO) -> [(HTTP.Method, a -> Wai.Request -> env Wai.Response)] -> API
-- makeEndpoints =

{-
data API where
  Endpoint :: Path a => (m ~> IO) -> HTTP.Method -> (a -> Wai.Request -> m Wai.Response) -> API
  Route :: Path a => (m ~> IO) -> [(HTTP.Method, a -> Wai.Request -> m Wai.Response)] -> API
  -- Route :: Path a => (m ~> IO) -> (HTTP.Method -> a -> Wai.Request -> m Wai.Response) -> API
  Scope :: { prefix :: [Text], middleware :: Wai.Middleware, children :: [API] } -> API
  -- Dir :: [Text] -> API TODO: For serving files
-}

{-
appify :: API -> Wai.Application -> Wai.Application
appify api catchAll request respond = case api of
  Endpoint @isPath transformation method handler -> do
    if method == Wai.requestMethod request
    then case fromPath @isPath $ Wai.pathInfo request of
            Left _ -> catchAll request respond
            Right pathData -> do
              response <- transformation $ handler pathData request
              respond response
    else catchAll request respond
  Route @isPath transformation handlerMap -> do
    case lookup (Wai.requestMethod request) handlerMap of
      Nothing -> catchAll request respond
      Just handler -> case fromPath @isPath $ Wai.pathInfo request of
        Left _ -> catchAll request respond
        Right pathData -> do
          response <- transformation $ handler pathData request
          respond response
  Scope _ _ [] -> catchAll request respond
  Scope prefix middleware apis -> do
    let maybeNewPath = stripPrefix prefix $ Wai.pathInfo request
    case maybeNewPath of
      Nothing -> catchAll request respond
      Just newPath -> do
        let newRequest = request {Wai.pathInfo = newPath}
        chooseAPI apis catchAll
  _ -> catchAll request respond
-}
-- chooseAPI :: [API] -> Wai.Application -> Wai.Application
-- chooseAPI catchAll request respond
convert :: forall a. Typeable a => String
convert = show . typeRep $ Proxy @a

instance FromPath Person where
  route = undefined

{-
class Path a where
  -- pattern Path :: a -> [Text]
  path :: DSL Expr State Error a

-- data METHOD where
--   HTTP.GET :: METHOD
--   HTTP.POST :: METHOD
--   HTTP.PUT :: METHOD

data Method :: HTTP.StdMethod -> Type where
  GET :: Method HTTP.GET
  POST :: Method HTTP.POST
  PUT :: Method HTTP.PUT

-- cmpMethod :: Method a -> Method b -> Bool
-- cmpMethod Get Get = True
-- cmpMethod Post Post = True
-- cmpMethod Put Put = True
-- cmpMethod Get Post = False
-- cmpMethod _ _ = False

class IsStdMethod (m :: HTTP.StdMethod) where
  toStdMethod :: HTTP.StdMethod

instance IsStdMethod HTTP.GET where
  toStdMethod = HTTP.GET

instance IsStdMethod HTTP.POST where
  toStdMethod = HTTP.POST

instance IsStdMethod HTTP.PUT where
  toStdMethod = HTTP.PUT

-- class (IsStdMethod m, Path a) => IsEndpoint (m :: METHOD) a where

class (Monad env, IsStdMethod m, Path a) => IsEndpoint env (m :: HTTP.StdMethod) a | a -> env where
  endpointHandler :: Wai.Request -> a -> env Wai.Response

class (Monad env, Path a) => IsRoute env a | a -> env where
  routeHandler :: Wai.Request -> HTTP.StdMethod -> a -> env Wai.Response

instance IsRoute IO Animal where
  routeHandler = undefined

type Env = Int

-- data API where
--   Router :: forall env a. IsRoute env a => { transformation :: env ~> IO } -> API
--   Endpointer :: forall env method a. IsEndpoint env method a => { transformation :: env ~> IO } -> API
--   Scope :: { path :: [Text], middleware :: Wai.Middleware, children :: [API] } -> API

data API where
  Route :: forall env a. IsRoute env a => (env ~> IO) -> API
  Endpoint :: forall env method a. IsEndpoint env method a => (env ~> IO) -> API
  Scope :: { prefix :: [Text], middleware :: Wai.Middleware, children :: [API] } -> API

route :: forall env a. IsRoute env a => (env ~> IO) -> API
route = Route @env @a

endpoint :: forall env method a. IsEndpoint env method a => (env ~> IO) -> API
endpoint = Endpoint @env @method @a

get :: forall env a. IsEndpoint env HTTP.GET a => (env ~> IO) -> API
get = Endpoint @env @HTTP.GET @a

post :: forall env a. IsEndpoint env HTTP.POST a => (env ~> IO) -> API
post = Endpoint @env @HTTP.POST @a

put :: forall env a. IsEndpoint env HTTP.PUT a => (env ~> IO) -> API
put = Endpoint @env @HTTP.PUT @a

scope :: [Text] -> Wai.Middleware -> [API] -> API
scope = Scope

data Person = Person Text Int

instance Path Person where
  path = undefined

data Animal = Animal

instance Path Animal where
  path = undefined

instance IsEndpoint IO HTTP.GET Person where
  endpointHandler = undefined

instance IsEndpoint IO HTTP.POST Person where
  endpointHandler = undefined

instance IsEndpoint IO HTTP.POST Animal where
  endpointHandler = undefined

instance IsEndpoint IO HTTP.PUT Animal where
  endpointHandler = undefined

{-
findHandler :: Method m -> [Text] -> [Handler] -> Maybe Handler
findHandler m p [] = Nothing
findHandler m p ((Handler @env @method @route t f):hs) = case method @method of
  Get -> findHandler m p hs
  _ -> Nothing
-}

myAPI :: API
myAPI = scope [] id
  [ get @IO @Person id
  , post @IO @Animal id
  , put @IO @Animal id
  , route @IO @Animal id
  , scope ["api"] id
      [ get @IO @Person id
      , post @IO @Animal id
      , scope ["v2"] id
          [ post @IO @Animal id
          ]
      ]
  ]

{-
data API where
  Route :: forall env a. IsRoute env a => (env ~> IO) -> API
  Endpoint :: forall env method a. IsEndpoint env method a => (env ~> IO) -> API
  Scope :: { prefix :: [Text], middleware :: Wai.Middleware, children :: [API] } -> API
-}

appify :: API -> Wai.Application -> Wai.Application
appify (Route @env @a transformation) defaultApp request respond = do
  case HTTP.parseMethod $ Wai.requestMethod request of
    Left _ -> defaultApp request respond
    Right stdMethod -> do
      let pathParser = path @a
          pathResult = fst $ exec (Wai.pathInfo request) pathParser
      case pathResult of
        Left _ -> defaultApp request respond
        Right x -> do
          response <- transformation $ routeHandler @env @a request stdMethod x
          respond response
appify (Endpoint @env @method @a transformation) defaultApp request respond = do
  case HTTP.parseMethod $ Wai.requestMethod request of
    Left _ -> defaultApp request respond
    Right stdMethod ->
      if toStdMethod @method == stdMethod
      then do
        let pathParser = path @a
            pathResult = fst $ exec (Wai.pathInfo request) pathParser
        case pathResult of
          Left _ -> defaultApp request respond
          Right x -> do
            response <- transformation $ endpointHandler @env @method @a request x
            respond response
      else defaultApp request respond
appify (Scope prefix middleware []) defaultApp request respond = defaultApp request respond
appify (Scope prefix middleware children) defaultApp request respond = do
  let reqPath = Wai.pathInfo request
  case matchPrefix prefix reqPath of
    Nothing -> defaultApp request respond
    Just prefix -> do
      let trimmedReqPath = remove prefix reqPath
          candidates = filterAPIsByMethod children
      case candidates of
        [] -> defaultApp request respond
        (c:cs) -> case c of
          Route @env @a transformation -> do
            let pathParser = path @a
                pathResult = fst $ exec (Wai.pathInfo request) pathParser
            case pathResult of
              Left _ -> undefined
              Right res -> case HTTP.parseMethod $ Wai.requestMethod request of
                Left _ -> defaultApp request respond -- TODO: This shouldn't really happen I think? If so, not good.
                Right stdMethod -> middleware (\request' respond' -> do
                  response <- transformation $ routeHandler @env @a request' stdMethod res
                  respond' response) $ (request {Wai.pathInfo = trimmedReqPath}) respond
          Endpoint @env @method @a transformation -> do
            let pathParser = path @a
                pathResult = fst $ exec (Wai.pathInfo request) pathParser
            case pathResult of
              Left _ -> undefined
              Right res -> middleware (\request' respond' -> do
                response <- transformation $ endpointHandler @env @method @a request' res
                respond' response) $ (request {Wai.pathInfo = trimmedReqPath}) respond
          scoper@(Scope prefix' middleware' children') -> do

appify _ _ _ _ = undefined

filterAPIsByMethod :: HTTP.StdMethod -> [API] -> [API]
filterAPIsByMethod method apis = apis

remove :: [Text] -> [Text] -> [Text]
remove [] [] = []
remove [] path = path
remove prefix [] = [] -- TODO: Shouldn't be allowed
remove (p:ps) (p':ps') = p' : remove ps ps'

matchPrefix :: [Text] -> [Text] -> Maybe [Text]
matchPrefix prefix path = if Prelude.length prefix > Prelude.length path
  then Nothing
  else case (prefix, path) of
    (p:ps, p':ps') -> if p == p'
      then case matchPrefix ps ps' of
        Nothing -> Nothing
        Just pp -> Just (p : pp)
      else Nothing
    (ps, []) -> Nothing
    ([], ps') -> Just []
    ([], []) -> Just []
-}