# Build A Web Framework in Haskell From Scratch

## Haskell for Backend Web Development

## What is WAI?

## The Simplest Possible Server

```haskell
server :: Request -> Response
```

## Interacting With The Real World

```haskell
server :: Monad m => Request -> m Response
```

```haskell
server :: Request -> Identity Response
```

##  Making Our Server Modular

```haskell
server :: Reader Request Response
```

## Separating Effects

Once concern with the Okapi monad is that I can interleave random `IO` actions in the route parser. This means the programmer has to be careful of where `IO` actions are executed. Once an `IO` action is executed, it can't be undone. Even with backtracking. In practice, we want to keep our route parser, and handler (which might use `IO`) separate.

1. The Router - The one and only job of the router is to **extract and verify the existence of data in the request**.
2. The Handler - The one and only job of the handler is to **accept data provided by the router and generate a response in the desired context**.

In this way, we achieve separation of concerns. What does this look like?

```haskell
server
  :: Monad m
  => Router a          -- Router
  -> (a -> m Response) -- Handler
  -> (m a -> IO a)     -- Lifter
  -> Application       -- Application
server = undefined
```

```haskell
data Router a = Router
  { 
  }

server
  :: Monad m
  => Router a          -- Router
  -> (a -> m Response) -- Handler
  -> (m ~> IO)         -- Lifter (Natural Transformation)
  -> Application       -- Application
server = undefined

serverPure
  :: (Request -> a)
  -> (a -> Response)
  -> Application
serverPure = undefined

serverPure'
  :: (Request -> Response)
  -> Application
serverPure' = undefined
```

### A Simpler Routing Interface

```haskell
server
  :: (RouteData -> Route -> m Response)
  -> (m ~> IO)
  -> Application
server f nt = ...
```

```haskell
data Route = Route
  { method :: Method
  , path :: [Text]
  }

pattern GetUser :: UserID -> Route
pattern GetUser userID = Route GET ["users", userID]

pattern PostUser :: UserID -> Route
pattern PostUser userID = Route POST ["users", userID]

server
  :: RouteData
  -> Route %1
  -> m Response
server routeData = \case
  GetUser userID -> do
    ...
  PostUser userID -> do
    ...
  _ -> return notFoundResponse
```

```haskell
-- Record of higher order functions
data RouteData = RouteData
  { queryParam :: HttpApiData a => Text -> Result a
  , header :: HttpApiData a => Text -> Result a
  , body :: ...
  , file :: ...
  , formParam :: ...
  , ...
  }

server
  :: RouteData
  -> Route %1
  -> m Response
server routeData route = do
  let
    setup1 = do
	  ...
	setup2 = ...
  setup3 <- ...
  case route of
    GetUser userID -> do
      ...
    PostUser userID -> do
      ...
    _ -> return notFoundResponse
```

### Alternate Syntax

#### Fast API Like

```haskell
getUsers :: Controller
getUsers = [get|
    /users
    ?age:Int
    ?name:Text
    ?status:Status
  |] id handler
  where
    handler :: (Int, Text, Status) -> m Response
    handler = ...

[post| /user/:UserID |] :: ...

[put| /user/:UserID |]
```

#### Controller Method

```haskell
data Error = JSONError ... | ...
data Result a = Cont a | Next

data Extractor a = ...

instance Applicative Extractor where

extractUser :: Extractor User
extractUser = do
  methodIs GET
  pathParamIs @Text "users" <|> pathParamIs "people"
  userID <- pathParam @UserID
  userQuery <- json @UserQuery
  pure GetUser{..}

data Extractor a = Ok a | Fail

data Result a = Respond a | Next

type Handler m a = Extractor a -> m (Result Response)

controller
  :: (m ~> IO)
  -> Extractor a
  -> Handler m a
  -> Controller
controller transformer router handler = ...

combineController
  :: Controller
  -> Controller
  -> Controller
combineController c1 c2 = ...
```

```haskell
data Controller = Controller
  { 
  }
```

### Mixing Patterns with Extractors

Use patterns for method and path. Use extractors for everything else.

```haskell
router :: Route -> Extractor a
router = \case
  (GET, ["index"]) -> do
	..
  (GET, ["posts", PathParam postID]) -> do
	..
  _ -> undefined
```

Probably not ideal because the exact extractor value can depend on the path parameter. We can't guarantee the developer won't do this.

### Route as Data

```haskell
myRoute :: Endpoint
myRoute = Endpoint
  { method = GET
  , path =
	  [ Static "people"
	  , Param @PersonID "personID"
	  ]
  , query =
	  [ Param @Bool "profile"
	  ]
  , headers =
	  [ Param @Text "X-Some-Header"
	  ]
  , body = JSON @PersonFilter
  }
```

Combine with extractor DSL?

```haskell
myRoute :: Endpoint pd qd hd bd
myRoute = Endpoint
  { method = GET
  , path = do
      static "profile"
      pID <- param @ProfileID
      pure pID
  , query = do
	  useProfile <- flag "profile"
	  pure useProfile
  , headers = NoHeaders
  , body = json @PersonFilter
  }

myRoute' :: Endpoint pd qd hd bd rd
myRoute' = Endpoint
  { method = GET :| [PUT, POST]
  , path = do
      static "profile"
      pID <- param @ProfileID
      pure pID
  , query = do
       useProfile <- flag "profile"
	   pure useProfile
  , headers = NoHeaders
  , body = do
	  filter <- json @PersonFilter
	  pure filter
  , responder = do
	  sendOk <- ok
	  sendNotFound <- notFound
	  pure Send{..}
  }

myRoute'' :: Endpoint pd qd hd bd rd
myRoute'' = Endpoint
  GET
  static "index"
  NoQuery
  NoHeaders
  NoBody
  ok

data Params pd qd hd bd rd = Params
  { path :: pd
  , query :: qd
  , headers :: hd
  , body :: bd
  , response :: rd %1
  -- TODO: Have two fields for response ~
  -- On Error and on Ok
  -- , responseError :: red %1
  }

-- Use type level function to produce types for both
-- Endpoint and Params.

myHandler
  :: Monad m
  => (Params pd qd hd bd rd) %1
  -> m (Action Response)
myHandler paramsResult = case paramsResult of
  Error err -> do
	-- | Do logging or whatever if error
	liftIO $ print err
    return Next
  Ok params -> do
	let
      profileID     = path params
      isProfileView = query params
      personFilter  = body params

	return $ params.response.respondOk responseValue

makeController
  :: Monad m
  => (m ~> IO)
  -> Endpoint pd qd hd bd rd
  -> (Params pd qd hd bd rd -> m Response)
  -> Controller
makeController lifter endpoint handler = ...
```

The above seems to be the best design.

### Combining Controllers

####  Non-Empty List

```haskell
type Server = NonEmptyList Controller
-- Use Map instead

myServer = controller1 :| [controller2, controller3]

genApplication
  :: ServerOptions
  {-| Control body max size
	  , default response
	  , IO error to response
	  , etc.
  -}
  -> Server
  -> Application

genJSClient :: Server -> FilePath -> IO ()

genOpenAPISpec :: Server -> OpenAPISpec
```

`genApplication` takes server options and a server definition.



## Megalith Web Framework

### File-based Routing

Megalith supports file-based routing. Placing a `.ml` file in your project's `pages` directory will automatically generate a route to that page. `.ml` files can contain plain HTML. Here's an example `.ml` file called `index.ml`:

```html
<div>
  <h1>
    Welcome to my website.
  </h1>
  <p>
    This website was built using the Megalith web framweork.
  </p>
</div>
```

If we run this app and go to `localhost:3000/index`, this page will be rendered in our browser.

### Nested File-based Routes

We may create nested routes by simply creating a directory in our `pages` directory.  For example, if we create a `products` directory in the `pages` directory, and then put `bolts.ml` in the `products` directory, our app will have the route `localhost:3000/products/bolts`.

### Dynamic Routes

We can also create dynamic routes that contain parameters. We can use these route parameters in our templates. To do this, we need to wrap the file/directory name in square brackets (`[]`). We can then use the name inside the square brackets to refer to the parameter in our templates. Here's an example:

```html
<!-- pages/products/[category].ml -->
<div>
  <h1>This is the $(category::Text) category.</h1>
</div>
```

Maybe consider using `!` instead of `[]` for dynamic routes.

Running the app and going to `localhost:3000/pages/products/watches` will render the page:

```html
<div>
  <h1>This is the watches category.</h1>
</div>
```

### Template Syntax

Pushup like approach:

```haskell
myHTML :: HTML
myHTML =
  <ul>
    ^forEach [1..10] \n ->
       <li>Number: ^n</li>
  </ul>
``` 

Or, a more traditional approach:

```haskell
myHTML :: HTML
myHTML =
  <ul>
    {list}
  </ul>
  where
    list = forEach [1..10] \n -> <li>Number: {n}</li>
```

Megalith includes a GHC plugin that introduces a literal syntax for HTML tags. Inspired by JSX and Phoenix Components.

### Components

```haskell
type Component a = a -> HTML

class Component a where
  render :: a -> HTML
```

### Routes

```haskell
get
  :: Parser a
  -> (a -> m Response)
  -> ???
```

```haskell
type Application m a = (Parser a, a -> m Response)
```

### Server Pages

```haskell
-- pages/index.mli --> localhost:3000/index

<div>
  <h1>Welcome to the Home Page!</h1>
</div>
```

```haskell
-- pages/products/[category].mli
import Data.Text

<div>
  
</div>
```

```haskell
Plan.Plan
  { lifter = id,
    endpoint =
      Endpoint.Endpoint
        { method = GET,
          path = do
            Path.static "index"
            magicNumber <- Path.param @Int
            pure magicNumber,
          query = do
            x <- Query.param @Int "x"
            y <- Query.option 10 $ Query.param @Int "y"
            pure (x, y),
          headers = pure (),
          body = pure (),
          responder = do
            itsOk <- Responder.json
              @Int
              HTTP.status200
              do
                addSecretNumber <- AddHeader.using @Int "X-SECRET"
                pure addSecretNumber
            pure itsOk
        },
    handler = \(Params.Params magicNumber (x, y) () () responder) -> do
      let newNumber = magicNumber + x * y
      print newNumber
      return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
  }
```

```haskell
Plan.Plan
  { -- Identity function as the lifter.
    lifter = id,

    -- Define the endpoint for the web service.
    endpoint =
      Endpoint.Endpoint
        { -- HTTP GET method for this endpoint.
          method = GET,

          -- Path pattern for this endpoint.
          path = do
            -- Expect "index" as a static part of the path.
            Path.static "index"

            -- Capture an integer parameter from the path.
            magicNumber <- Path.param @Int
            pure magicNumber,

          -- Query parameters for this endpoint.
          query = do
            -- Capture an integer query parameter named "x".
            x <- Query.param @Int "x"

            -- Capture an optional integer query parameter named "y" with a default value of 10.
            y <- Query.option 10 $ Query.param @Int "y"
            pure (x, y),

          -- No specific headers expected for this endpoint.
          headers = pure (),

          -- No request body expected for this endpoint.
          body = pure (),

          -- Define the responder for this endpoint.
          responder = do
            -- Create a JSON responder with HTTP status 200 and an integer value.
            itsOk <- Responder.json @Int HTTP.status200
              do
                -- Check for the presence of an "X-SECRET" header with an integer value.
                addSecretNumber <- AddHeader.using @Int "X-SECRET"
                pure addSecretNumber

            -- Return the configured responder.
            pure itsOk
        },

    -- Define the handler function for the web service.
    handler = \(Params.Params magicNumber (x, y) () () responder) -> do
      -- Calculate a new number based on the magicNumber, x, and y.
      let newNumber = magicNumber + x * y

      -- Print the new number to the console.
      print newNumber

      -- Return a response with the new number and an additional header based on the new number.
      return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
  }

```


```haskell
Plan.Plan
  { lifter = id,
    endpoint = Endpoint.Endpoint
      GET
      do
        Path.static "index"
        magicNumber <- Path.param @Int
        pure magicNumber
      do
        x <- Query.param @Int "x"
        y <- Query.option 10 $ Query.param @Int "y"
        pure (x, y)
      pure ()
      pure ()
      do
        itsOk <- Responder.json @Int HTTP.status200
          do
            addSecretNumber <- AddHeader.using @Int "X-SECRET"
            pure addSecretNumber
        pure itsOk
    handler = \(Params.Params magicNumber (x, y) () () responder) -> do
      let newNumber = magicNumber + x * y
      print newNumber
      return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
  }
```

```haskell
Plan
  Endpoint
    Method.GET
    Path.do
      Path.static "index"
      magicNumber <- Path.param @Int
      Path.pure magicNumber
    Query.do
      x <- Query.param @Int "x"
      y <- Query.option 10 $ Query.param @Int "y"
      Query.pure (x, y)
    Headers.pure ()
    Body.pure ()
    Responder.do
      itsOk <- Responder.json @Int HTTP.status200
        AddHeader.do
          addSecretNumber <- AddHeader.using @Int "X-SECRET"
          AddHeader.pure addSecretNumber
      Responder.pure itsOk
  \magicNumber (x, y) () () responder -> do
    let newNumber = magicNumber + x * y
    print newNumber
    return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
  id
```

```haskell
Plan $$
  Endpoint $$
    Method.GET
    Path.do
      Path.static "index"
      magicNumber <- Path.param @Int
      Path.pure magicNumber
    Query.do
      x <- Query.param @Int "x"
      y <- Query.option 10 $ Query.param @Int "y"
      Query.pure (x, y)
    Headers.pure ()
    Body.pure ()
    Responder.do
      itsOk <- Responder.json @Int HTTP.status200
        AddHeader.do
          addSecretNumber <- AddHeader.using @Int "X-SECRET"
          AddHeader.pure addSecretNumber
      Responder.pure itsOk
  \magicNumber (x, y) () () responder -> do
    let newNumber = magicNumber + x * y
    print newNumber
    return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
  id
```

```haskell
Plan $$
  Endpoint $$
    GET
    do
      Path.static "index"
      magicNumber <- Path.param @Int
      pure magicNumber
    do
      x <- Query.param @Int "x"
      y <- Query.option 10 $ Query.param @Int "y"
      pure (x, y)
    do pure ()
    do pure ()
    do
      itsOk <- Responder.json @Int HTTP.status200 do
        addSecretNumber <- AddHeader.using @Int "X-SECRET"
        AddHeader.pure addSecretNumber
      pure itsOk
  \magicNumber (x, y) () () responder -> do
    let newNumber = magicNumber + x * y
    print newNumber
    return $ responder (\addHeader response -> addHeader (newNumber * 100) response) newNumber
  id
```

### Endpoint Patterns

```haskell
data Request = Request StdMethod [Text] Query BS.ByteString RequestHeaders
data Server m r = Server
  { responder :: Responder r
  , handler :: Request -> r -> m Response
  }

pattern GetUsers :: Maybe Filter -> Request
pattern GetUsers optFilter <- Request
  GET
  ["users"]
  (Query.eval filterQuery -> Ok filter)
  ""
  _
  
pattern AddUser :: User -> Request
pattern AddUser user <- Request
  POST
  ["users"]
  _
  (Body.eval (json @User) -> Ok user)
  _

pattern GetUsersByID :: UserID -> MatcherInput
pattern GetUsersByID userID <- Request
  GET
  (Path.eval pathParams -> Ok userID)
  _
  ""
  _
  where
    pathParams = do
      Path.static "users"
      userID <- Path.param @UserID "userID"
      pure userID

myServer :: MyResponderType -> Request -> IO Response 
myServer res = \case
  GetUser -> do
    ...
  GetUserByID userID -> do
    ...
  AddUser user -> do
    ...
  _ -> do
    ...

myServer = Server myResponder myServer

spend :: a %1 -> a %m ????
```
