{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Okapi.Effect.Request.Path where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.Effect.Log as Log
import qualified Okapi.State.Request.Path as Path
import qualified Okapi.Type.Failure as Failure
import qualified Web.HttpApiData as Web

type MonadPath m = (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Logger.MonadLogger m, Path.MonadState m)

-- $pathParsers
--
-- These are the path parsers.

-- | Parses and discards mutiple path segments matching the values and order of the given @[Text]@ value
path :: MonadPath m => m [Text.Text]
path = Combinators.many pathParam

pathPart :: MonadPath m => Text.Text -> m ()
pathPart part = pathParam `Failure.is` part

-- | Parses and discards a single path segment matching the given @Text@ value
pathParam :: (Web.FromHttpApiData a, MonadPath m) => m a
pathParam = do
  Log.logIt "Parsing path parameter"
  maybePathSeg <- Path.gets safeHead
  case maybePathSeg of
    Nothing -> do
      Log.logIt "There are no more path parameters left"
      Failure.next
    Just pathSeg -> do
      Log.logIt $ "Got path segment: " <> pathSeg
      Path.modify $ Prelude.drop 1
      case Web.parseUrlPieceMaybe pathSeg of
        Nothing -> do
          Log.logIt "Couldn't parse path parameter as Type"
          Failure.next
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
      Failure.next

look :: MonadPath m => m a -> m a
look action = do
  request <- Path.get
  result <- action
  Path.put request
  pure result
