{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Path
  ( MonadPath (..),
    State (..),
    gets,
    modify,
    look,
    Path,
    path,
    pathPart,
    pathParam,
    pathEnd,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Okapi.Error as Error
import qualified Okapi.Log as Log
import qualified Web.HttpApiData as Web

type MonadPath m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

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

-- $pathParsers
--
-- These are the path parsers.

-- | Parses and discards mutiple path segments matching the values and order of the given @[Text]@ value
path :: MonadPath m => m [Text.Text]
path = Combinators.many pathParam

pathPart :: MonadPath m => Text.Text -> m ()
pathPart part = pathParam `Error.is` part

-- | Parses and discards a single path segment matching the given @Text@ value
pathParam :: (Web.FromHttpApiData a, MonadPath m) => m a
pathParam = do
  Log.logIt "Parsing path parameter"
  maybePathSeg <- gets safeHead
  case maybePathSeg of
    Nothing -> do
      Log.logIt "There are no more path parameters left"
      Error.next
    Just pathSeg -> do
      Log.logIt $ "Got path segment: " <> pathSeg
      modify $ Prelude.drop 1
      case Web.parseUrlPieceMaybe pathSeg of
        Nothing -> do
          Log.logIt "Couldn't parse path parameter as Type"
          Error.next
        Just value -> do
          Log.logIt "Parsed path parameter"
          pure value
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
pathEnd :: MonadPath m => m ()
pathEnd = do
  Log.logIt "Checking if there are any more path parameters"
  currentPath <- path
  if List.null currentPath
    then do
      Log.logIt "There are path parameters"
      pure ()
    else do
      Log.logIt "There are no more path parameters"
      Error.next
