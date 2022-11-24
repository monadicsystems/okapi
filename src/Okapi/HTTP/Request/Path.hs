{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.HTTP.Request.Path
  ( Parser (..),
    Path,
    parse,
    match,
    slash,
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
import qualified Okapi.HTTP.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Path
import qualified Okapi.Log as Log
import qualified Web.HttpApiData as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- $pathParsers
--
-- These are the parse parsers.

-- | Parses and discards mutiple parse segments matching the values and order of the given @[Text]@ value
parse :: Parser m => m Path
parse = Combinators.many param

match :: Parser m => Path -> m ()
match path = do
  path' <- parse
  Monad.guard (path == path')

-- TODO: Add matchAll :: [Text.Text] -> m ()
slash :: Parser m => Text.Text -> m ()
slash text = do
  text' <- param
  Monad.guard (text == text')

-- | Parses and discards a single parse segment matching the given @Text@ value
param :: (Web.FromHttpApiData a, Parser m) => m a
param = do
  Log.logIt "Parsing parse parameter"
  maybePathSeg <- gets safeHead
  case maybePathSeg of
    Nothing -> do
      Log.logIt "There are no more parse parameters left"
      Error.next
    Just pathSeg -> do
      Log.logIt $ "Got parse segment: " <> pathSeg
      modify $ Prelude.drop 1
      case Web.parseUrlPieceMaybe pathSeg of
        Nothing -> do
          Log.logIt "Couldn't parse parse parameter as Type"
          Error.next
        Just value -> do
          Log.logIt "Parsed parse parameter"
          pure value
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
end :: Parser m => m ()
end = do
  Log.logIt "Checking if there are any more parse parameters"
  currentPath <- parse
  if List.null currentPath
    then do
      Log.logIt "There are parse parameters"
      pure ()
    else do
      Log.logIt "There are no more parse parameters"
      Error.next
