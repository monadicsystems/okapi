{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text
import Okapi

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Text.InterpolatedString.Perl6

data Recipe = Recipe
  { recipeName :: Text
  , recipeIngredients :: [Text]
  , recipeDetail :: Text
  , recipeCookTime :: Int -- ^ Cook time of the recipe in minutes
  }

pattern HomeRoute = (GET, [""]) -- ^ @[""]@ represents the trailing slash e.g. www.example.com/

pattern QueryRecipesRoute = (GET, ["recipes"])

pattern PostRecipesRoute = (POST, ["recipes"])
  
renderURL :: (Method, Path) -> Text
renderURL (_, p) = renderPath p

renderFormAttrs :: (Method, Path) -> Text
renderFormAttrs (m, p) = renderAction p <> " " <> renderMethod m
  where
    renderAction p = "action=\"" <> renderPath p <> "\""
    renderMethod = \case
      POST -> "method=\"" <> "post" <> "\""
      _    -> "method=\"" <> "get" <> "\"" -- ^ method="get" is the default method for forms

methodAndPath :: MonadOkapi m => m (Method, Path)
methodAndPath = do
  m <- method
  p <- path
  return (m, p)

main :: IO ()
main = run id $ route methodAndPath $ \case
  HomeRoute -> do
    let html =
          [qq|
            <h1>Recipes Home</h1>
            <hr>
            <h2>Query Recipes</h2>
            <form {renderFormAttrs QueryRecipesRoute}>
              ...
            </form>
            <hr>
            <h2>Create New Recipes</h2>
            <form {renderFormAttrs PostRecipesRoute}>
              ...
            </form>
          |]
    return $ setHTML html $ ok
  QueryRecipesRoute -> do
    return $ setJSON True $ ok
  PostRecipesRoute -> do
    return $ setJSON True $ ok
  _ -> next
