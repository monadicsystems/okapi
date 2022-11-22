{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Internal.Request.Query
  ( State (..),
    gets,
    modify,
    look,
    Query,
    Item,
    Value (..),
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
import qualified Okapi.Internal.Error as Error
import qualified Web.HttpApiData as Web

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

type Query = [Item]

type Item = (Text.Text, Value)

data Value = Param Text.Text | Flag deriving (Eq, Show) -- QueryList [Text]
