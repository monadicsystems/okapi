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


