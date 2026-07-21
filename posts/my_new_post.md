# Elegant HTTP

Haskell is the best language for elegantly describing HTTP APIs. Let me show you.

```haskell
import qualified Okapi.HTTP
import qualified Okapi.HTTP.Request as Req
import qualified Okapi.HTTP.Response as Res
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai

helloServer = Server -- (1)
  { transform = id -- (2)
  , middleware = id -- (3)
  , contract = Req.base :-> Res.base -- (4)
  , function = fn \(req, _raw) -> do -- (5)
      print req.method
      print req.path
      return (Res.cons 200 [] (return "Hello world!"))
  }

backup :: Wai.Application
backup = undefined -- (6)

main = Warp.run 8080 (route helloServer backup) -- (7)
```

1.
2.
3.
4.
5.
6.
7.

This is the simplest server you can define with `okapi`. Let's dig deeper into the *contract* field since everything else in okapi, like servers, clients, documentation, and more, is derived from it.

```haskell
Req.base :-> Res.base
```

This is the most general contract you can define. It accepts all forms of HTTP request, and may return any form of HTTP response.

Contracts are just regular Haskell terms, so you can query the types of contracts using `:t` in ghci.

```haskell
>>> :t Req.base
Req.base
  :: Req.Contract
       Okapi.HTTP.Request.Method.Base
       Okapi.HTTP.Request.Path.Base
       Okapi.HTTP.Request.Query.Base
       Okapi.HTTP.Headers.Base
       Okapi.HTTP.Body.Base
```

A *request contract* is indexed by 5 type parameters, each one representing a part of a request. The *base request contract* uses the same types used in `wai`'s `Request` record for each respective slot. Okapi provides type synonyms, each one called `Base`, to refer to these underlying types. The `Base` type for method is `Method` from `http-types`, the `Base` type for path is `[Text]`, and so on. 

The same idea applies to *response contracts*.

```haskell
>>> :t Res.base
Res.base
  :: Res.Contract
       Okapi.HTTP.Response.Status.Base
       Okapi.HTTP.Headers.Base
       Okapi.HTTP.Body.Base
```

Notice that both the request and response base contracts use the same `Base` type for the headers and body.

To associate a request contract with a single response contract, the `:->` constructor is used. It is analogous to the `->` operator used for defining function types in programming, or implications in logic. If request, then response.

```haskell
>>> :t Req.base :-> Res.base
Req.base :-> Res.base
  :: HTTP
       (Signature
          Okapi.HTTP.Request.Method.Base
          Okapi.HTTP.Request.Path.Base
          Okapi.HTTP.Request.Query.Base
          Okapi.HTTP.Headers.Base
          Okapi.HTTP.Body.Base
          (Okapi.Response.Data.Response
             Okapi.HTTP.Response.Status.Base
             Okapi.HTTP.Headers.Base
             Okapi.HTTP.Body.Base))
```

It combines the information provided by a request contract and response contract to define a possible interaction with the server from end-to-end. This is isn't very useful because the base contracts are unconstrained. Without constraints, there is no information. The above contract matches all requests, and can return any response.

Okapi currently exposes two ways to constrain contracts. Using combinators is one way.

```haskell
import Data.Function ((&))

myRequest
  = Req.base
  & Req.method Method.Get
  & Req.path do
      lit "greet"
      name <- seg text "name"
      pure name
```

Using Haskell's record update syntax is the other.

```haskell
myRequest = Req.base
  { Req.method = Method.method Method.Get
  , Req.path = do
      lit "greet"
      name <- seg text "name"
      pure name
  }
```

We'll be using the record update syntax for the rest of this writing. Now, if we query the type of `myRequest` in ghci we'll see that the type is updated too.

```haskell
>>> :t myRequest
myRequest
  :: Req.Contract
       Okapi.HTTP.Request.Method.GET
       Data.Text.Text
       Okapi.HTTP.Request.Query.Base
       Okapi.HTTP.Headers.Base
       Okapi.HTTP.Body.Base
```

The method slot is fixed to `GET`, and the path slot is of type `Text`. The other slots remain unconstrained.

Response contracts can be updated in the same way.

```haskell
myResponse = Res.base
  { Res.status = Status.status 200
  , Res.headers = Headers.contentType Headers.PlainText
  , Res.body = Body.plaintext 
  }
```

The type of `myResponse` reflects the updates we've made to the base response contract record.

```haskell
>>> :t myResponse
myResponse
  :: Res.Contract
       Okapi.HTTP.Response.Status.S200
       ()
       (IO Data.Text.Text)
```

Method and status are set with a single values like `Get` or `200`, so instead of having to update it manually, okapi provides base values where these slots are already filled. For example, these definitions suffixed with `'` are equivalent to the non-suffixed versions above.

```haskell
myRequest' = Req.get
  { Req.path = do
      lit "greet"
      name <- seg text "name"
      pure name
  }

myResponse' = Res.ok
  { Res.headers = Headers.contentType Headers.PlainText
  , Res.body = Body.plaintext 
  }
```


There's a common misconception in the Haskell community that the more code you push into the type system, the better. The most popular example of this mindset is `servant`, a library that provides a type-level DSL for describing HTTP APIs.

```haskell

```

`servant` uses type families and typeclasses to interpret these type definitions as servers, clients, type-safe URL builders, OpenAPI documentation, and more. The maintainers of `servant` have gone great lengths to hide the complexity needed to make these interpretations work, so the end user doesn't need to be an expert type-level programmer, but it can still leak and leads to frustration.

- The more logic that's pushed to the type-level, the longer compile times are
- Error messages are harder to understand
- Harder to understand in general; 

When I first released `okapi` a while ago, I would get comments like

> i'd be reluctant to use something that didn't have Servant's ability to treat an API as a single type, just because you can do so much interesting stuff with it automatically.






Backend frameworks in other programming languages use comments, decorators, macros, or  Theoretically, it's a cool idea, but practically, there are

