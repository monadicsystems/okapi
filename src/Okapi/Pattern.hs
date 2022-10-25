{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Pattern where

import qualified Control.Monad.Combinators as Combinators
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Okapi.Effect.Request as Request
import qualified Okapi.Effect.Server as Server
import qualified Okapi.Type.Request as Request
import qualified Web.HttpApiData as Web

-- $routing
--
-- Okapi implements routes and type-safe relative URLs using bidirectional pattern synonyms and view patterns.
-- Routing can be extended to dispatch on any property of the request, including method, path, query, headers, and even body.
-- By default, Okapi provides a @route@ function for dispatching on the path of the request.

route :: Request.RequestM m => m a -> (a -> m ()) -> m ()
route parser dispatcher = parser >>= dispatcher

viewQuery :: Text.Text -> Request.Query -> (Maybe Request.QueryValue, Request.Query)
viewQuery name query = case List.lookup name query of
  Nothing -> (Nothing, query)
  Just value -> (Just value, List.delete (name, value) query)

viewQueryParam :: Web.FromHttpApiData a => Text.Text -> Request.Query -> (Maybe a, Request.Query)
viewQueryParam name query = case List.lookup name query of
  Just (Request.QueryParam param) -> case Web.parseQueryParamMaybe param of
    Nothing -> (Nothing, query)
    Just value -> (Just value, List.delete (name, Request.QueryParam param) query)
  _ -> (Nothing, query)

-- $relativeURLs
--
-- Relative URLs are useful when we want to refer to other locations within our app.
-- Thanks to bidirectional patterns, we can use the same pattern to deconstruct an incoming request
-- AND construct the relative URL that leads to itself.

data RelURL = RelURL Request.Path Request.Query

-- TODO: Use ToURL typeclass for Path and Query, then combine for RelURL??
renderRelURL :: RelURL -> Text.Text
renderRelURL (RelURL path query) = case (path, query) of
  ([], []) -> ""
  ([], q) -> "?" <> renderQuery q
  (p, []) -> renderPath p
  (p, q) -> renderPath p <> "?" <> renderQuery q

renderPath :: Request.Path -> Text.Text
renderPath [] = "/"
renderPath (pathSeg : path) = "/" <> pathSeg <> loop path
  where
    loop :: Request.Path -> Text.Text
    loop [] = ""
    loop (pathSeg : path) = "/" <> pathSeg <> loop path

renderQuery :: Request.Query -> Text.Text
renderQuery [] = ""
renderQuery ((name, Request.QueryFlag) : query) = name <> "&" <> renderQuery query
renderQuery ((name, Request.QueryParam value) : query) = name <> "=" <> value <> "&" <> renderQuery query

parseRelURL :: Text.Text -> Maybe RelURL
parseRelURL possibleRelURL = Either.eitherToMaybe $
  flip Atto.parseOnly possibleRelURL $ do
    path <- Combinators.many pathSeg
    maybeQueryStart <- Combinators.optional $ Atto.char '?'
    case maybeQueryStart of
      Nothing -> pure $ RelURL path []
      Just _ -> do
        query <- Combinators.many queryParam
        pure $ RelURL path query
  where
    pathSeg :: Atto.Parser Text.Text
    pathSeg = do
      Atto.char '/'
      Atto.takeWhile (\c -> c /= '/' && c /= '?')

    queryParam :: Atto.Parser (Text.Text, Request.QueryValue)
    queryParam = do
      queryParamName <- Atto.takeWhile (\c -> c /= '=' && c /= '&')
      mbEquals <- Combinators.optional $ Atto.char '='
      case mbEquals of
        Nothing -> pure (queryParamName, Request.QueryFlag)
        Just _ -> do
          queryParamValue <- Atto.takeWhile (/= '&')
          pure (queryParamName, Request.QueryParam queryParamValue)
