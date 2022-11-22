{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Query
  ( Parser (..),
    Item (..),
    Value (..),
    use,
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
import qualified Okapi.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Query
import qualified Okapi.Log as Log
import qualified Web.HttpApiData as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $queryParsers
--
-- These are the use parsers.

use :: Parser m => m Query
use = do
  Log.logIt "Parsing the request use"
  use <- get
  put []
  Log.logIt "Request use parsed"
  pure use

value :: Parser m => Text.Text -> m Value
value queryItemName = do
  Log.logIt $ "Getting a use value with the name: " <> queryItemName
  maybeQueryItem <- gets (List.find (\(queryItemName', _) -> queryItemName == queryItemName'))
  case maybeQueryItem of
    Nothing -> do
      Log.logIt $ "Couldn't find use value with the name: " <> queryItemName
      Error.next
    Just queryItem@(_, value) -> do
      Log.logIt $ "Found use value: " <> show value
      modify $ List.delete queryItem
      pure value

-- | Parses the value of a use parameter with the given type and name
param :: (Web.FromHttpApiData a, Parser m) => Text.Text -> m a
param queryItemName = do
  Log.logIt "Parsing use parameter"
  queryItemValue <- value queryItemName
  case queryItemValue of
    Flag -> do
      Log.logIt $ queryItemName <> " is a use flag"
      Error.next
    Param valueText -> do
      Log.logIt $ "Parsed use parameter as Text: " <> valueText
      case Web.parseQueryParamMaybe valueText of
        Nothing -> do
          Log.logIt "Couldn't parse use parameter as Type"
          Error.next
        Just value -> do
          Log.logIt "Parsed use parameter"
          pure value

-- | Test for the existance of a use flag
flag :: Parser m => Text.Text -> m ()
flag queryItemName = do
  Log.logIt "Parsing use flag"
  queryItemValue <- value queryItemName
  case queryItemValue of
    Flag -> do
      Log.logIt "Parsed use flag"
      pure ()
    _ -> do
      Log.logIt $ queryItemName <> " isn't a use flag or doesn't exist in the use"
      Error.next

params :: (Web.FromHttpApiData a, Parser m) => Text.Text -> m (NonEmpty.NonEmpty a)
params name = do
  Log.logIt $ "Parsing list of use parameters with the name: " <> name
  Combinators.NonEmpty.some . param $ name

end :: Parser m => m ()
end = do
  Log.logIt "Checking if use is empty"
  currentQuery <- use
  if List.null currentQuery
    then do
      Log.logIt "Query is empty"
      pure ()
    else do
      Log.logIt "Query is not empty"
      Error.next
