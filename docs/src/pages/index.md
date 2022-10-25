---
title: Getting started
pageTitle: Okapi - A micro web framework for Haskell
description: Build any web application using a minimal set of operations and combinators.
---

Learn how to get Okapi set up in your project and start building for the Web. {% .lead %}

{% quick-links %}

{% quick-link title="Project setup" icon="installation" href="/#project-setup" description="Step-by-step guides to using Okapi in your latest Haskell project." /%}

{% quick-link title="Todo App Tutorial" icon="presets" href="/docs/todo-app" description="Learn how to use Okapi by building a simple todo app." /%}

{% quick-link title="Servant integration" icon="plugins" href="/servant-integration" description="Use Okapi with your Servant API easily." /%}

{% quick-link title="Learn Haskell" icon="theming" href="/intro-to-haskell" description="New to Haskell? Take our official Haskell course before learning Okapi." /%}

{% /quick-links %}

---

## Project setup

You can bring Okapi into your Haskell project using Stack, Cabal, or Nix. Here are the directions for each method.

### Stack

1. Add `okapi` to your project's `package.yaml` file:

    ```yaml
    dependencies:
    - base >= 4.7 && < 5
    - aeson
    - text
    - okapi
    ```

2. (Optional) Add the latest commit hash of the Okapi GitHub repo to your `stack.yaml` file, under `extra-deps`:

    ```yaml
    extra-deps:
    - git: https://github.com/monadicsystems/okapi.git
      commit: 826225af458d5e9c28d6e6eed5df468489638a3a
    ```

    {% callout type="warning" title="Warning" %}
      The commit hash used in the example above will be outdated. Make sure you check for the correct commit hash [here]().
    {% /callout %}

3. Run the command `stack build` to make sure your project builds.

4. Add an `import Okapi` statement to your modules:

    ```haskell
    module MyModule where

    import Okapi

    ...
    ```

### Cabal

Coming soon

### Nix

Coming soon

---

## Basic usage

A server takes a request and returns the appropriate response. In Okapi, the correct response for any
given request is decided by *extracting data from* or *verifying properties of* the request using **parsers**.

### Types

The core type of the Okapi library is `ServerT m a`.

```haskell
newtype ServerT m a = ServerT {runServerT :: Except.ExceptT Failure (State.StateT State m) a}
  deriving newtype
    ( Except.MonadError Failure,
      State.MonadState State
    )
```

Okapi also exports the type constraint `ServerM m`, which is the abstract interface of `ServerT m`.

```haskell
type ServerM m =
  ( Functor m,
    Applicative m,
    Applicative.Alternative m,
    Monad m,
    Monad.MonadPlus m,
    Except.MonadError Failure m,
    State.MonadState State m
  )
```

We recommend using the type constraint instead of the concrete type to annotate your parsers.

```haskell
myParser :: ServerT (ReaderT AppConfig IO) (Int, Char)
myParser = ... -- Concrete not good. Lot's of boilerplate code for unwrapping and wrapping values. 

myParser' :: (ServerM m, MonadIO m, MonadReader AppConfig m) => m (Int, Char)
myParser' = do
  ... -- Abstract good. Less boilerplate. Can reuse and test easily.
```

Your top level parser definition will probably need a concrete type annotation for your program to compile. To start, we recommend just using `IO`.

```haskell
module Main where

import Okapi

type Server = ServerT IO -- Can parse HTTP requests + perform I/O

main :: IO ()
main =
  run
    id
    -- ^ This argument is a function for changing an effectful computation `m a` into `IO a`.
    --   In this case it's just `id` since `id :: IO a -> IO a`
    myServer

myServer :: Server Response
myServer = myParser1 <|> myParser2

myParser1 :: (ServerM m, MonadIO m) => m Response
myParser1 = do
  logIO "Using handler 1"
  ...

myParser2 :: ServerM m => m Response
myParser2 = ...

logIO :: MonadIO m => String -> m ()
logIO msg = ...
```

See [Custom monad stack]() for more information on how to integrate your custom monad stack with
this library.

### Parsers

Okapi provides **parsers** to extract data from or verify properties of HTTP requests. They all have a
return type of `ServerM m => m a`, where `a` is some value.

Parsers can either **succeed** or **fail**.

For example, the `methodGET` parser succeeds if the HTTP request has the GET method, otherwise it fails.

Parsers in Okapi are analagous to what most other web frameworks would call *routes*, but parsers are more granular and modular.

There is a category of parsers for each component of an HTTP request:

1. Method parsers
   
   These parsers are for parsing the request method and are prefixed with `method-`.

   Examples: `method`, `methodGET`, `methodPOST`, `methodPATCH`, `methodOPTIONS`

2. Path parsers
   
   These parsers are for parsing the request path (excluding the query string) and are prefixed with `path-`.

   Examples: `path`, `pathParam`

3. Query parsers
   
   These parsers are for parsing the request query and are prefixed with `query-`.

   Examples: `query`, `queryParam`, `queryFlag`

4. Body parsers

   These parsers are for parsing the request body and are prefixed with `body-`.

   Examples: `body`, `bodyJSON`, `bodyURLEncoded`

5. Header parsers

   These parsers are for parsing the request headers and are prefixed with `header-` or `cookie-`.

   Examples: `headers`, `header`, `cookie`, `cookieCrumb`

To learn more about each parser, I recommend reading the [haddock documentation]() for each parser and looking at the examples.

### Combining Parsers

There are two ways to combine parsers and create more complex ones:

1. Sequencing

   You can execute one parser after another parser using `do` notation:

   ```haskell
   getUser = do
     methodGET                 -- Check that the request method is GET
     pathParam `is` "users"    -- Check that the first path parameter is "users"
     uid <- pathParam @UserID  -- Bind second path parameter to `uid` as a `UserID`
     return $ setJSON uid $ ok -- Respond with 200 OK and the user's ID
   ```

   You can also use `>>` and `>>=`:

   ```haskell
   getUser = methodGET >> pathParam `is` "user" >> pathParam @UserID >>= \uid -> return $ setJSON uid $ ok
   ```

   A parser composed of a sequence of other parsers ONLY succeeds if all of the parsers in the sequence succeed. If any one of the parsers in the sequence fails, the entire parser fails.

   For example, the `getUser` parser defined above would fail if the HTTP request was `POST /users/5`, because the `methodGET` parser would fail. If the HTTP request was `GET /guests/9`, `getUser` would fail because the ``pathParam `is` "users"`` parser would fail.

   In summary, given parsers `p1` and `p2`, the parser `p3` defined as:

    ```haskell
    p3 = do
      p1
      p2
    ```

   succeeds if `p1` AND `p2` succeed. `p1` and `p2` MAY be different types.

2. Branching

   You can create a parser that tries multiple parsers using the `<|>` operator from `Control.Applicative.Alternative`:

   ```haskell
   pingpong = ping <|> pong

   ping = do
     methodGET
     pathParam `is` "ping"
     return $ setJSON "pong" $ ok

   pong = do
     methodGET
     pathParam `is` "pong"
     return $ setJSON "ping" $ ok
   ```

   A parser composed of a set of parsers using the `<|>` operator succeeds if ANY of the parsers succeed.

    If all of the parsers in the composition fail, the entire parser fails.

    Using the `pingpong` parser as an example, if the incoming HTTP request was `GET /ping`, `pingpong` would succeed because the `ping` parser in `ping <|> pong` succeeds. If the incoming request was `GET /pong`, `pingpong` would still succeed. Eventhough `ping` in `ping <|> pong` would fail, `pong` would succeed, so `pingpong = ping <|> pong` succeeds.

    In summary, given parsers `p1` and `p2`, the parser `p3` defined as:

    ```haskell
    p3 = p1 <|> p2
    ```

    succeeds if `p1` OR `p2` succeed. `p1` and `p2` MUST be the same type.

### Failure

There are two functions that you can use to throw an error and terminate the parser: `next` and `throw`.
The difference between the two mainly has to do with how they affect the behavior of the `<|>` operator.

In short, given parsers `p1` and `p2`, and a parser `p3` defined as `p3 = p1 <|> p2`:

1. If `p1` fails with `next`, `p2` is tried next.
2. If `p1` fails with `throw`, `p2` isn't tried. Instead a response is returned immediately.

As an example, let's say we have a simple calculator API defined like so:

```haskell
getXY :: Okapi (Int, Int)    -- (1)
getXY = do
  x <- pathParam
  y <- pathParam
  pure (x, y)

divide :: Okapi Response     -- (2)
divide = do
  pathParam `is` "div"
  (x, y) <- getXY
  if y == 0
    then next
    else return $ setJSON (x / y) $ ok

multiply :: Okapi Response   -- (3)
multiply = do
  pathParam `is` "mul"
  (x, y) <- getXY
  return $ setJSON (x * y) $ ok

calculator :: Okapi Response -- (4)
calculator = do
  methodGET
  divide <|> multiply
```

If the path parameter assigned to `y` is equal to `0`, then the `divide` parser will fail and the `multiply` parser is tried next because `next` is used.

If the `divide` parser was defined like this,

```haskell
divide :: Okapi Response
divide = do
  pathParam `is` "div"
  (x, y) <- getXY
  if y == 0
    then throw $ setJSON "Dividing by 0 is forbidden" $ forbidden
    else return $ setJSON (x / y) $ ok

calculator :: Okapi Response
calculator = do
  methodGET
  divide <|> multiply
```

the `calculator` parser would immediately return a `forbidden` error response if the path parameter assigned to `y` was equal 0. The `multiply` parser isn't tried.

Notice how `throw` takes a `Response` value as an argument. This is the response that is immediately returned by the parser when `throw` is called. No other parsers are tried.

If you want to try the next parser, even if `throw` is used in the first branch, you can use the `<!>` operator.

```haskell
divide :: Okapi Response
divide = do
  pathParam `is` "div"
  (x, y) <- getXY
  if y == 0
    then throw $ setJSON "Dividing by 0 is forbidden" $ forbidden
    else return $ setJSON (x / y) $ ok

calculator :: Okapi Response
calculator = do
  methodGET
  divide <!> multiply
```

If we feed the request `GET /div/5/0` to this definition of `calculator`, both `div` and `multiply` would be tried. `calculator` is defined using `<!>` instead of `<|>`, so even though `divide` calls `throw` when `y == 0`, the next parser `multiply` is tried anyway!

See [Error handling]() for more information on errors and how to handle them.

### Combinators

Combinators are functions that modify parsers. They are higher order parsers, meaning they take a parser as an argument and give you back a parser. Most of these combinators can be found in the `parser-combinators` library. To use it, just add `parser-combinators` to your dependencies and import
`Control.Monad.Combinators` into your modules. There are several, but the most used ones are `is`, `optional`, and `option` so let's cover those.

`is` is for comparing some data in the request to a value. It's mostly used for matching on the request path

```haskell
-- | Works for GET /books/fiction/sci-fi
bookstore = do
  methodGET
  pathParam `is` "books"
  pathParam `is` "fiction"
  pathParam `is` "sci-fi"
  sciFiBooks <- execDBQuery ...
  return $ setJSON sciFiBooks $ ok
```

, but can be used with any parser.

```haskell
-- | Works for GET /books/fiction?subgenre=sci-fi
bookstore = do
  methodGET
  pathParam `is` "books"
  pathParam `is` "fiction"
  queryParam "subgenre" `is` "sci-fi"
  sciFiBooks <- execDBQuery ...
  return $ setJSON sciFiBooks $ ok
```

Another important combinator is `optional`. `optional` allows you to handle a parser that fails, in your own way by turning the result of type `a` in to a result of type `Maybe a`. If the parser fails, it returns a `Nothing`. If it succeeds, it returns `Just x` where `x :: a`. Let's look at the example used
in the hero of this page, the `greet` server:

```haskell
-- | Works for /greet/<name> OR /greet?name=<name> OR /greet
greet = do
  methodGET
  pathParam @Text `is` "greet"
  maybeName <- optional (pathParam <|> queryParam "name")
  pathEnd
  let greeting = case maybeName of
        Nothing   -> "Hello there."
        Just name -> "Hello, " <> name <> "."
   return $ setPlainText greeting $ ok
```

Thanks to `optional`, if `pathParam <|> queryParam "name"` fails, we can provide a default value. In this case, we assign `greeting` to `"Hello there."` because we have no `name`.

Another way to catch failures and use a default value is by using `option`. Using `option`, we can modify the `greet` server defined above like this:

```haskell
-- | Works for /greet/<name> OR /greet?name=<name> OR /greet
greet = do
  methodGET
  pathParam @Text `is` "greet"
  name <- option "Stranger" (pathParam <|> queryParam "name")
  pathEnd
  let greeting = "Hello, " <> name <> "."
   return $ setPlainText greeting $ ok
```

`option` takes a default value as its first argument, and a parser. If the parser passed into `option` succeeds, a value is returned as usual. If the parser fails then no value can be returned by the parser, so the default value passed into `option` is used instead. For example, if we sent the request `GET /greet`
to the `greet` server defined using `option`, we'd get `"Hello, Stranger."` because we don't give it a `name` parameter and it uses the default `"Stranger"`.

See [Parser combinators]() for more information on combinators and how to use them.

---

## Getting help

Click on the GitHub icon in the upper right corner to go to the repository and submit an issue.
You should be able to get someone to help you out this way.
A Discord channel, or probably some other communication channel, is coming soon.
