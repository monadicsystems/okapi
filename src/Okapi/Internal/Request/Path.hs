{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Internal.Request.Path
  ( State (..),
    gets,
    modify,
    look,
    Path,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Okapi.Internal.Error as Error
import qualified Web.HttpApiData as Web

class Monad m => State m where
  get :: m Path
  put :: Path -> m ()

gets :: State m => (Path -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Path -> Path) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: State m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result

type Path = [Text.Text]
