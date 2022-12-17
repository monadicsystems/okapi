{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Query
  ( Parser (..),
    Query,
    Item,
    Value,
    parse,
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
import qualified Web.HttpApiData as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $queryParsers
--
-- These are the parse parsers.

parse :: Parser m => m Query
parse = do
  query <- get
  put []
  pure query

value :: Parser m => Text.Text -> m Value
value queryItemName = do
  maybeQueryItem <- gets (List.find (\(queryItemName', _) -> queryItemName == queryItemName'))
  case maybeQueryItem of
    Nothing -> do
      Error.next
    Just queryItem@(_, value) -> do
      modify $ List.delete queryItem
      pure value

-- | Parses the value of a parse parameter with the given type and name
param :: (Web.FromHttpApiData a, Parser m) => Text.Text -> m a
param queryItemName = do
  queryItemValue <- value queryItemName
  case queryItemValue of
    Flag -> do
      Error.next
    Param valueText -> do
      case Web.parseQueryParamMaybe valueText of
        Nothing -> do
          Error.next
        Just value -> do
          pure value

-- | Test for the existance of a parse flag
flag :: Parser m => Text.Text -> m ()
flag queryItemName = do
  queryItemValue <- value queryItemName
  case queryItemValue of
    Flag -> do
      pure ()
    _ -> do
      Error.next

params :: (Web.FromHttpApiData a, Parser m) => Text.Text -> m (NonEmpty.NonEmpty a)
params name = do
  Combinators.NonEmpty.some . param $ name

end :: Parser m => m ()
end = do
  currentQuery <- parse
  if List.null currentQuery
    then do
      pure ()
    else do
      Error.next
