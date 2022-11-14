{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Okapi.Effect.Request.Query where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.Effect.Log as Log
import qualified Okapi.State.Request.Query as Query
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request
import qualified Web.HttpApiData as Web

type MonadQuery m = (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Logger.MonadLogger m, Query.MonadState m)

-- $queryParsers
--
-- These are the query parsers.

query :: MonadQuery m => m Request.Query
query = do
  Log.logIt "Parsing the request query"
  query <- Query.get
  Query.put []
  Log.logIt "Request query parsed"
  pure query

queryValue :: MonadQuery m => Text.Text -> m Request.QueryValue
queryValue queryItemName = do
  Log.logIt $ "Getting a query value with the name: " <> queryItemName
  maybeQueryItem <- Query.gets (Foldable.find (\(queryItemName', _) -> queryItemName == queryItemName'))
  case maybeQueryItem of
    Nothing -> do
      Log.logIt $ "Couldn't find query value with the name: " <> queryItemName
      Failure.next
    Just queryItem@(_, queryValue) -> do
      Log.logIt $ "Found query value: " <> show queryValue
      Query.modify $ List.delete queryItem
      pure queryValue

-- | Parses the value of a query parameter with the given type and name
queryParam :: (Web.FromHttpApiData a, MonadQuery m) => Text.Text -> m a
queryParam queryItemName = do
  Log.logIt "Parsing query parameter"
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    Request.QueryFlag -> do
      Log.logIt $ queryItemName <> " is a query flag"
      Failure.next
    Request.QueryParam valueText -> do
      Log.logIt $ "Parsed query parameter as Text: " <> valueText
      case Web.parseQueryParamMaybe valueText of
        Nothing -> do
          Log.logIt "Couldn't parse query parameter as Type"
          Failure.next
        Just value -> do
          Log.logIt "Parsed query parameter"
          pure value

-- | Test for the existance of a query flag
queryFlag :: MonadQuery m => Text.Text -> m ()
queryFlag queryItemName = do
  Log.logIt "Parsing query flag"
  queryItemValue <- queryValue queryItemName
  case queryItemValue of
    Request.QueryFlag -> do
      Log.logIt "Parsed query flag"
      pure ()
    _ -> do
      Log.logIt $ queryItemName <> " isn't a query flag or doesn't exist in the query"
      Failure.next

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
      Failure.next

look :: MonadQuery m => m a -> m a
look action = do
  request <- Query.get
  result <- action
  Query.put request
  pure result
