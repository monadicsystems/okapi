## Introduction

Okapi is a microframework for web development in Haskell based on monadic parsing.
The inspiration for Okapi originally came from looking at web frameworks in other language ecosystems such as Python’s Flask,
Nim’s Jester, OCaml’s Dream, and F#’s Giraffe, which the name of this Haskell framework is related to.
I noticed that many Haskell web frameworks tend to require a lot of boilerplate code, and/or make use of a lot of advanced type level language
features that make it hard to understand the internals of the framework. The goal of Okapi is to create a Haskell web framework
with an ergonomic developer experience that is idiomatic to the host language.

## Parsers

<!-- Haskell is considered one of the best, if not the best, programming languages for implementing parsers. -->
In Haskell, a simple `String` parser can be modeled as a function with the type `String -> (Either ParserError a, String)`.
This function takes values of type `String` and returns either a `ParserError` (if it fails) or a value of some type `a` (if it succeeds), along with a new `String`
that's missing the characters that were consumed by the parsing function. We could use the function like so:

```haskell
```

This is great, but issues start to arise when we try to compose parsers with other parsers. For example, let's say we wanted to parse blah blah:

```haskell
```

To avoid the explicit passing of state from one parser to the next, we can use monads. You may have already noticed that the type of our parser can be simplified with the `State String` monad.
We can transform our function of type `String -> (Either ParseError a, String)` to a value of type `State String (Either ParserError a)`. A value of type `State String (Either ParserError a)`
represents a value of type `Either ParserError a` that was computed in a stateful context, where the state is of type `String`. Now that our parser is defined as a monad we can use `do` notation
, and it becomes easier to compose our parsers with other parsers because we don't have to manually pass the state from a previous parser to the next one.
Let's try the parser composition we tried above with our new parser definition:

```haskell
```

<!-- To complete the simplification of our parser, the last thing we need to do is combine our `State` and `Except` monads using monad transformers.
Monad transformers allow us to combine the properties of multiple monads. -->
<!-- We can further simplify our parser by pushing the `Either ParserError a` part of the type signature into the parser's context using another monad.  -->
As you can see our parsers compose a lot better, but we still have to explicitly handle the result of the parsers because they may return a `ParserError`.
Functions that return values of the type `Either ParserError a` can be modelled using the `Except ParserError` monad. A value of the type `Except ParserError a`
represents a value of type `a` that is computed in a context that may not succeed, but instead throw an error value of type `ParserError`. In our case we want
our parser's computations to happen in a context in which there is state of type `String`, and the possibilty of throwing an error value of type `ParserError`.
To get both of these useful abilities, let's combine the `Except ParserError` monad with our `State String` monad using monad transformers. Our simplified parser
now has the type `ExceptT ParserError (State String a)`, where `ExceptT` is a monad transformer that gives our base `State String` monad the ability to throw error
values of type `ParserError` upon failure. To make the code examples easier on our eyes, let's make a type synonym defined as `type Parser a = ExceptT ParserError (State String a)`.
Now, any value anottated with the type `Parser a` represents a value of some type `a` that is computed in a context that has access to state of type `String` AND may throw error
values of type `ParserError` upon failing. Let's redefine the example we defined above:

```haskell
```

Great.

## HTTP Request Parsers

Now, let's redefine `type Parser a = ExceptT ParserError (State String a)` as `type Parser a = ExceptT HTTPError (State HTTPRequest a)`. This is an HTTP Request Parser. Instead of parsing happening in a context where the computation has access to state of type `String` and can throw errors of type `ParserError`, it happens in a context where the computation has access to state of type `HTTPRequest` and can throw errors of type `HTTPError`. Just like the string parser above had a concept of "consuming" parts of a `String`, the HTTP request parser "consumes" values of the type `HTTPRequest`. By consume we mean .... If you break values of type `String` into its smallest consituents, you get values of type `Char`. A `String` value is a list of `Char` values. What are the smallest constituents of a `HTTPRequest` value? The data type `HTTPRequest` is defined as follows:

```haskell
data HTTPRequest = HTTPRequest
  { method :: Method
  , path :: [Text]
  , query :: Query
  , body :: ByteString
  , headers :: Headers
  }
```

<!-- Just like our basic string parser in the beginning removed the part of the string it parsed before using the next parser.
 -->
Our HTTP request parser consumes different parts of the HTTP request like the `method` and `query`. Once a piece of the HTTP request is parsed, it is removed from the request before it is implicitly passed to the next parser.

There are 2 types of parsers:

1. Data parsers
2. Checker parsers

There are 5 types of parsers for each of the 5 parts of a HTTP request.

1. Method Parsers

```haskell
method :: MonadOkapi m => m Method

matchMethod :: MonadOkapi m => Method -> m ()

get :: MonadOkapi m => m ()
get = matchMethod "GET"
```

2. Path Parsers

```haskell
path :: MonadOkapi m => m Path -- Parses entire remaining path
path = many seg

seg :: MonadOkapi m => m Text

matchPath :: MonadOkapi m => Path -> m ()
matchPath desiredPath = mapM_ matchSeg desiredPath

matchSeg :: MonadOkapi m => Text -> m ()

pathParam :: MonadOkapi m => FromHttpApiData a => m a

pathEnd :: MonadOkapi m => m ()
```

4. Query Parsers

```haskell
query :: MonadOkapi m => m Query -- parses entire query

queryParam :: MonadOkapi m => FromHttpApiData a => Text -> m a

queryFlag :: MonadOkapi m => Text -> m ()

queryParamRaw :: Text -> m Text
```

6. Body Parsers

```haskell
body :: MonadOkapi m => m Body

bodyJSON :: MonadOkapi m, FromJSON a => m a

bodyFormURLEncoded :: FromForm a, MonadOkapi m => m a

bodyFormMultipart :: FromForm a, MonadOkapi m => m (a, [File])
```

8. Headers Parsers

```haskell
headers :: MonadOkapi m => m Headers

header :: MonadOkapi m => HeaderName -> m Header

cookie :: MonadOkapi m => m Cookie

crumb :: MonadOkapi m => Text -> m Crumb
```

We can use these to create increasingly complex parsers. For example, let's say we wanted to implement a HTTP parser that matches the request `GET /blog`. That would look like this:

```haskell
blogRoute :: Parser ()
blogRoute = do
  get            -- Make sure that the request is a GET request
  matchSeg "blog" -- Match against the path segment /blog
  pathEnd        -- Make sure that there are no more path segments remaining in the request
```

Just like earlier, with our monadic string parser, we can sequence HTTP request parsers using `do` notation. This request parser isn't really useful though because it doesn't return anything. Let's make it return a response:

```haskell
blogRoute :: Parser HTTPResponse
blogRoute = do
  get
  matchSeg "blog"
  pathEnd
  return ok
```

Now if we run our parser, it will return a `200 OK` response if we send a `GET` request to the `/blog` endpoint. On top of being able to sequence parsers with `do` notation thanks to `Parser` being an instance of the `Monad` typeclass, we can also build parsers that "choose" between multiple subparsers. This is possible because the `Parser` type is also an instance of the `Alternative` typeclass, which provides the `<|>` operator.

Explain `<|>` then explain we can also parser combinators like `many`, `some`, `optional`, `option`, `takeWhile`, etc.

Then explain the two types of errors and how to throw and catch them.

Then explain returning responses and executing a parser.

Explaining type safe URLs with patterns:

## Patterns

Okapi uses bi-directional patterns to have typesafe urls. So you would have something like:

```haskell
-- Matches routes of the form /blog/99
pattern BlogRoute :: Int -> Path
pattern BlogRoute uuid <- ["blog", PathParam uuid]
  where
    BlogRoute uuid = ["blog", PathParam uuid]
```

or just

```haskell
-- Bidriectional Implicit
pattern BlogRoute :: Int -> Path
pattern BlogRoute uuid = ["blog", PathParam uuid]

pattern BlogCategoryRoute :: Text -> Path
pattern BlogCategoryRoute category = ["blog", PathParam category]
```

uses these bidrectional patterns with the `route` parser, like so:

```haskell
route :: MonadOkapi m => (Path -> m Response) -> m Response
route matcher = do
  path <- parsePath
  matcher path
  
myAPI :: MonadOkapi m => m Response
myAPI = route $ \case
  BlogRoute uuid -> do
    get
    return ok
  BlogRouteCategory category -> do
    get
    mbOrderBy <- optional $ queryParam @Order "order"
    case mbOrderBy of
      Nothing -> do
        ...
        return ok
      Just orderBy -> do
        ...
        return ok
  _ -> next
```

Since both routes are `GET` requests, let's factor out the `get` parser:

```haskell
myAPI :: MonadOkapi m => m Response
myAPI = do
  get
  route $ \case
    BlogRoute uuid -> return ok
    BlogRouteCategory category -> do
      mbOrderBy <- optional $ queryParam @Order "order"
      case mbOrderBy of
        Nothing -> do
          ...
          return ok
        Just orderBy -> do
          ...
          return ok
    _ -> next
```

## URLs

There are two types of URLs that you can generate with Okapi:

1. Relative URLs
2. Absolute URLs

```haskell
data RelURL = RelURL Path Query
data AbsURL = AbsURL Scheme Host (Maybe Port) RelURL
```
