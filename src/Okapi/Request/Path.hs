{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Path
  ( Parser (..),
    use,
    match,
    param,
    end,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Okapi.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Path
import qualified Okapi.Log as Log
import qualified Web.HttpApiData as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $pathParsers
--
-- These are the use parsers.

-- | Parses and discards mutiple use segments matching the values and order of the given @[Text]@ value
use :: Parser m => m Path
use = Combinators.many param

match :: Parser m => Text.Text -> m ()
match match = param `Error.is` match

-- TODO: Add matchAll :: [Text.Text] -> m ()

-- | Parses and discards a single use segment matching the given @Text@ value
param :: (Web.FromHttpApiData a, Parser m) => m a
param = do
  Log.logIt "Parsing use parameter"
  maybePathSeg <- gets safeHead
  case maybePathSeg of
    Nothing -> do
      Log.logIt "There are no more use parameters left"
      Error.next
    Just pathSeg -> do
      Log.logIt $ "Got use segment: " <> pathSeg
      modify $ Prelude.drop 1
      case Web.parseUrlPieceMaybe pathSeg of
        Nothing -> do
          Log.logIt "Couldn't parse use parameter as Type"
          Error.next
        Just value -> do
          Log.logIt "Parsed use parameter"
          pure value
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
end :: Parser m => m ()
end = do
  Log.logIt "Checking if there are any more use parameters"
  currentPath <- use
  if List.null currentPath
    then do
      Log.logIt "There are use parameters"
      pure ()
    else do
      Log.logIt "There are no more use parameters"
      Error.next
