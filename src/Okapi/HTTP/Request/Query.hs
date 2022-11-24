{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.HTTP.Request.Query
  ( Parser (..),
    Query (..),
    Item (..),
    Value (..),
    parse,
    value,
    param,
    flag,
    params,
    end,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Okapi.HTTP.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Query
import qualified Okapi.Log as Log
import qualified Web.HttpApiData as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $queryParsers
--
-- These are the parse parsers.

parse :: Parser m => m Query
parse = do
  Log.logIt "Parsing the request parse"
  query <- get
  put []
  Log.logIt "Request parse parsed"
  pure query

value :: Parser m => Text.Text -> m Value
value queryItemName = do
  Log.logIt $ "Getting a parse value with the name: " <> queryItemName
  maybeQueryItem <- gets (List.find (\(queryItemName', _) -> queryItemName == queryItemName'))
  case maybeQueryItem of
    Nothing -> do
      Log.logIt $ "Couldn't find parse value with the name: " <> queryItemName
      Error.next
    Just queryItem@(_, value) -> do
      Log.logIt $ "Found parse value: " <> show value
      modify $ List.delete queryItem
      pure value

-- | Parses the value of a parse parameter with the given type and name
param :: (Web.FromHttpApiData a, Parser m) => Text.Text -> m a
param queryItemName = do
  Log.logIt "Parsing parse parameter"
  queryItemValue <- value queryItemName
  case queryItemValue of
    Flag -> do
      Log.logIt $ queryItemName <> " is a parse flag"
      Error.next
    Param valueText -> do
      Log.logIt $ "Parsed parse parameter as Text: " <> valueText
      case Web.parseQueryParamMaybe valueText of
        Nothing -> do
          Log.logIt "Couldn't parse parse parameter as Type"
          Error.next
        Just value -> do
          Log.logIt "Parsed parse parameter"
          pure value

-- | Test for the existance of a parse flag
flag :: Parser m => Text.Text -> m ()
flag queryItemName = do
  Log.logIt "Parsing parse flag"
  queryItemValue <- value queryItemName
  case queryItemValue of
    Flag -> do
      Log.logIt "Parsed parse flag"
      pure ()
    _ -> do
      Log.logIt $ queryItemName <> " isn't a parse flag or doesn't exist in the parse"
      Error.next

params :: (Web.FromHttpApiData a, Parser m) => Text.Text -> m (NonEmpty.NonEmpty a)
params name = do
  Log.logIt $ "Parsing list of parse parameters with the name: " <> name
  Combinators.NonEmpty.some . param $ name

end :: Parser m => m ()
end = do
  Log.logIt "Checking if parse is empty"
  currentQuery <- parse
  if List.null currentQuery
    then do
      Log.logIt "Query is empty"
      pure ()
    else do
      Log.logIt "Query is not empty"
      Error.next
