```haskell
{-
Haskell Server Pages are a nice way to create server responses.

You call them from your parsers like so:

hsp
  :: FilePath
  -> m Response
hsp rootDir = undefined

main :: IO
main = run id (hsp "/")

Will look for `.hsp` files in the root directory and serve them according to their path relative to the root directory.

home.hsp              -> /home
users.hsp             -> /users
users
  |- userID::Int.hsp  -> /user/userID::Int
  |- userID::Int
       |- posts.hsp   -> /user/userID::Int/posts
       |- follows.hsp -> /user/userID::Int/follows

This whole module is an example HSP.
-}
-- /store/category::Category
methodGET

mbColor <- optional $ queryParam @Color "color"
mbBrand <- optional $ queryParam @Brand "brand"
mbMinPrice <- optional $ queryParam @Float "min_price"
mbMaxPrice <- optional $ queryParam @Float "max_price"

products <- select conn do
  each product
  where_ ...

<html>
<head>
</head>
<body>
case category of
  Shoes   -> <h2>Shoes on SALE!</h2>
  Shirts  -> </>
  Dresses -> <h2>Beautiful DRESSES For SALE! Red Dresses 25% OFF!</h2>
  Pants   -> </>

for_ products \Product{..} ->
    <h1>Forever51 Clothing Store</h1>
    <p><%= productName %></p>
    
<%= footer %>
</body>
</html>
```
