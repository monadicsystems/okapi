# 🦓🦒Okapi

Okapi is a data-driven micro framework for implementing HTTP servers.

- Ergonomic DSLs for routing and parsing requests
- Integrate Okapi with ANY monad stack or effect system
- Automatically generate clients and OpenAPI specifications (coming soon)
- Programatically generate your API's structure

## Hello World Example

```haskell
helloWorld =
  responder @200 @'[] @Text.Text @Text.Text
    . method HTTP.GET id
    $ \greet _req -> return $ greet noHeaders "Hello World!"

main =
  Warp.run 8000
    . withDefault helloWorld
    $ \_ resp -> resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."
```

## Calculator Example

```haskell
data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Sq
    | Neg
    deriving (Show)

instance Web.FromHttpApiData Operator where
    parseUrlPiece "add" = Right Add
    parseUrlPiece "sub" = Right Sub
    parseUrlPiece "minus" = Right Sub
    parseUrlPiece "mul" = Right Mul
    parseUrlPiece "div" = Right Div
    parseUrlPiece "neg" = Right Neg
    parseUrlPiece "sq" = Right Sq
    parseUrlPiece "square" = Right Sq
    parseUrlPiece _ = Left "Can't parse operator..."

shared =
  lit "calc"
    . param @Operator
    . param @Int

unary =
  responder @200 @'[] @Text.Text @Int
    . responder @500 @'[] @Text.Text @Text.Text
    . method HTTP.GET id

unaryHandler operator x ok wrongArgs _req =
  return $ case operator of
    Sq  -> ok noHeaders (x * x)
    Neg -> ok noHeaders (x * (-1))
    _   -> wrongArgs noHeaders $ Text.pack (show operator) <> " needs two arguments."

binary =
  param @Int
    . responder @200 @'[] @Text.Text @Int
    . responder @500 @'[] @Text.Text @Text.Text
    . responder @403 @'[] @Text.Text @Text.Text
    . method HTTP.GET id

binaryHandler operator x y ok wrongArgs divByZeroErr _req =
  return $ case operator of
    Add -> ok noHeaders (x + y)
    Sub -> ok noHeaders (x - y)
    Mul -> ok noHeaders (x * y)
    Div ->
      if y == 0
      then divByZeroErr noHeaders "You can't divide by 0."
      else ok noHeaders (div x y)
    _ -> wrongArgs noHeaders $ Text.pack (show operator) <> " needs one argument."

calc :: Node '[]
calc = shared $ choice
  [ unary unaryHandler
  , binary binaryHandler
  ]

main =
  Warp.run 8003
    . withDefault calc
    $ \_ resp -> resp $ Wai.responseLBS HTTP.status404 [] "Not Found..."
```