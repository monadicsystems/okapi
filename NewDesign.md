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
  , body = JSON @PersonFilter
  }

myRoute' :: Endpoint pd qd hd bd rd
myRoute' = Endpoint
  GET
  do
    static "profile"
    pID <- param @ProfileID
    pure pID
  do
    useProfile <- flag "profile"
	pure useProfile
  NoHeaders
  do
	filter <- json @PersonFilter
	pure filter
  do
	respondOk <- ok
	respondNotFound <- notFound
	pure Responders{..}

data EndpointData pd qd hd bd rd = EndpointData
  { path :: pd
  , query :: qd
  , headers :: hd
  , body :: bd
  , response :: rd %1
  }

myHandler
  :: Monad m
  => (EndpointData pd qd hd bd rd) %1
  -> m Response
myHandler 

makeController
  :: Monad m
  => (m ~> IO)
  -> Endpoint pd qd hd bd rd
  -> (Result (EndpointData pd qd hd bd rd) -> m Response)
  -> Controller
makeController lifter endpoint handler = ...
```

The above seems to be the best design.

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