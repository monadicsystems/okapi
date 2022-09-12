and we want to be able to pattern match on the request method inorder to choose the correct handler for a POST request, or a PATCH request.

Type-safe URLs (or URIs) are URLs that can be constructed safely because they contain type information. Constructing URLs with string concatenation is error prone because the dynamic parts of the URL aren’t specifically typed, they are all of type String, and they are more prone to human error because typos within the String aren’t caught by the compiler. Let’s say our server handles the endpoint `/todo/:id`, where `:id` is a path parameter representing a todo ID number. If we want to link to the `/todo/:id` endpoint from one of our pages we need to construct the URL and add it to our HTML. With a plain string concatenation method, we could use a function like


```
renderTodoIDURL :: Text -> Text
renderTodoIDURL id = “/todo/” <> id
```

Which would be bad because we would be free to do something that doesn’t make sense, like `renderTodoIDURL “dog”` or `renderTodoIDURL “onehundred”`. To fix this we could do something like

```
renderTodoIDURL :: Int -> Text
renderTodoIDURL id = “/todo/” <> show id
```
Where the Text is replaced with an Int. Now we remove the possibility of doing anything like `renderTodoIDURL “abc”`. Even better would be replacing the Int with the newtype TodoID or Natural.

We’ve made the URL generation function more type safe which is great, but what happens if we update our handler to handle the endpoint `/task/:id` instead of `/todo/:id`? Well in order for our URL generation to work properly we would have to update the function `renderTodoIDURL` to

```
renderTodoIDURL :: Int -> Text -- We should change the name too, but let’s skip that for now
renderTodoIDURL id = “/task/” <> show id
```

Notice how if we change our handler, we have to make a change to the URL rendering function too. We have to make changes in at least two places. This is another downside to this method. If the handler and URL rendering function are defined close to each other this might not be bad, but we’d have to rely on developers to keep them close. Another downside is that we get no help from the compiler if we make this change. We could’ve updated the handler and forgot to update the URL rendering function, and deployed our app!
How Haskell Web Frameworks solve this issue

Various Haskell web frameworks solve this issue to various degrees using various methods. The two main language features that Haskell web frameworks use to solve the type-safe URL problem are:

Type-level programming
Template Haskell

How Okapi solves the problem

Okapi proposes using an underused language feature for solving this problem: bidrectional patterns.

Bidirectional patterns are patterns that can be used to deconstruct and construct data in Haskell.

Okapi provides a function called `route :: (Path -> Handler m) -> Handler m` that is meant to be used with the LambdaCase language extension to match a path to a `Handler m`. An example of it being used:

```
myServer = route $ \case
  TodoRoute -> …
  TodoByIDRoute todoID -> …
  _ -> skip
```

The same patterns that we use to pattern match on the path can be used to construct URLs as well.

Since the pattern is the deconstructor and constructor, if we update the pattern we’re updating the handler and the URL generator at the same time.

If the patterns are implicitly bidirectional, you only have to make a change in one place to update the deconstructor and constructor. Nice!

There are cases where you might have to make a pattern explicitly bidirectional. This means the deconstructor and constructor have different definitions, but still the same type. Updates to one may require a change in the other, so you will updating two places at most when you make a change like with our original handler and URL generation function. The good thing is that they are defined right next to each other and their placement in the source code is enforced by the compiler. Type mismatches will be caught by the compiler too. Value mismatches can slip through though if the constructor and deconstructor have the same type.

Dispatching on other parts of the Request

By default, Okapi exposes a route function that takes a function that dispatches the correct handler based on the path. Users of Okapi can create a custom route function that allows them to dispatch on any part of the request, like the method, query parameters, and/or headers. For example, let’s create a route function that matches against the request method, path, and query parameters.

Another upside to this approach to the type-safe URL problem is that it is extensible unlike other approaches. Most frameworks only allow matching on the request path. This all you need most of the time, but if you need the extra functionality for whatever reason, Okapi has it. Depending on the needs of the developer, they can dispatch on the different parts of the request they care about, and create patterns for them.
