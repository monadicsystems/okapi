{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Query
  ( MonadQuery (..),
    State (..),
    gets,
    modify,
    look,
    Query,
    QueryItem,
    QueryValue (..),
    query,
    queryValue,
    queryParam,
    queryFlag,
    queryParamList,
    queryEnd,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Okapi.Error as Error
import qualified Okapi.Log as Log
import qualified Web.HttpApiData as Web

type MonadQuery m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

class Monad m => State m where
  get :: m Query
  put :: Query -> m ()

gets :: State m => (Query -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Query -> Query) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: State m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result

type Query = [QueryItem]

type QueryItem = (Text.Text, QueryValue)

data QueryValue = QueryParam Text.Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

-- $queryParsers
--
-- These are the query parsers.

query :: MonadQuery m => m Query
query = do
  Log.logIt "Parsing the request query"
  query <- get
  put []
  Log.logIt "Request query parsed"
  pure query

queryValue :: MonadQuery m => Text.Text -> m QueryValue
queryValue queryItemName = do
  Log.logIt $ "Getting a query value with the name: " <> queryItemName
  maybeQueryItem <- gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName'))
  case maybeQueryItem of
    Nothing -> do
      Log.logIt $ "Couldn't find query value with the name: " <> queryItemName
      Error.next
    Just queryItem@(_, queryValue) -> do
      Log.logIt $ "Found query value: " <> show queryValue
      modify $ List.delete queryItem
      pure queryValue

-- | Parses the value of a query parameter with the given type and name
queryParam :: (Web.FromHttpApiData a, MonadQuery m) => Text.Text -> m a
queryParam queryItemName = do
  Log.logIt "Parsing query parameter"
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    QueryFlag -> do
      Log.logIt $ queryItemName <> " is a query flag"
      Error.next
    QueryParam valueText -> do
      Log.logIt $ "Parsed query parameter as Text: " <> valueText
      case Web.parseQueryParamMaybe valueText of
        Nothing -> do
          Log.logIt "Couldn't parse query parameter as Type"
          Error.next
        Just value -> do
          Log.logIt "Parsed query parameter"
          pure value

-- | Test for the existance of a query flag
queryFlag :: MonadQuery m => Text.Text -> m ()
queryFlag queryItemName = do
  Log.logIt "Parsing query flag"
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    QueryFlag -> do
      Log.logIt "Parsed query flag"
      pure ()
    _ -> do
      Log.logIt $ queryItemName <> " isn't a query flag or doesn't exist in the query"
      Error.next

queryParamList :: (Web.FromHttpApiData a, MonadQuery m) => Text.Text -> m (NonEmpty.NonEmpty a)
queryParamList name = do
  Log.logIt $ "Parsing list of query parameters with the name: " <> name
  Combinators.NonEmpty.some . queryParam $ name

queryEnd :: MonadQuery m => m ()
queryEnd = do
  Log.logIt "Checking if query is empty"
  currentQuery <- query
  if List.null currentQuery
    then do
      Log.logIt "Query is empty"
      pure ()
    else do
      Log.logIt "Query is not empty"
      Error.next
