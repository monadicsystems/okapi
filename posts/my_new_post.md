# Elegant HTTP

## Introduction

Haskell is the best language for elegantly describing HTTP APIs.

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

## Contracts for Great Good

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

We'll be using the record update syntax for the rest of the examples. Now, if we query the type of `myRequest` in ghci we'll see that the type is updated too.

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

Response contracts are updated in the same way.

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

Like we did with the base contracts, we can associate `myRequest` with `myResponse` to create a full end-to-end HTTP contract.

```haskell
>>> :t myReqeust :-> myResponse
myRequest :-> myResponse
  :: HTTP
       (Signature
          Okapi.HTTP.Request.Method.GET
          Data.Text.Text
          Okapi.HTTP.Request.Query.Base
          Okapi.HTTP.Headers.Base
          Okapi.HTTP.Body.Base
          (Okapi.Response.Data.Response
             Okapi.HTTP.Response.Status.S200
             ()
             (IO Data.Text.Text)))
```

The HTTP contract `Signature` is a combination of the types of the request and response contracts.

The form of HTTP contract used so far can only map a single request to a single response. What do we do if we want to return one of many responses? Use the split arrow constructor.

```haskell
>>> :t myReq :-< myRes
<ERROR>
```

Using `:-<` with just a single response contract results in a type error. Instead of a single response contract, the RHS of `:-<` must be multiple response contracts. One for each possible response that can be returned.

```haskell
myResponses = 
```

Then the question is, how do we combine multiple response contracts together?

1. Define a higher-kinded sum type that derives `Generic` and the `Responses` typeclass.
2. Use the variadic `responses` method provided by the `Responses` typeclass to build a responses contract

```haskell
data MyResponses f
	= IsOk (f Res.S200 Headers.Base Body.Base)
	| IsNotFound (f Res.404 Headers.Base Body.Base)
	| IsServerError (f Res.500 Headers.Base Body.Base)
	deriving (Generic, Responses)
```

Then the HTTP contract can use `:-<` with `responses`. The user must pass in one response contract for each constructor, in order of the constructors in the type definition.

```haskell
myResponses = responses
	Res.ok
	Res.notFound
	Res.serverError
```

Then, combining with `:-<`, we get the following.

```haskell
>>> :t myRequest :-< myResponses
myRequest :-< myResponses
  :: HTTP
       (Signature
          Okapi.HTTP.Request.Method.GET
          Data.Text.Text
          Okapi.HTTP.Request.Query.Base
          Okapi.HTTP.Headers.Base
          Okapi.HTTP.Body.Base
          (MyResponses Okapi.Response.Data.Response))
```
## Using Contracts

### Servers

### Clients

### Links

### OpenAPI Documentation

## Conclusion
