{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Request.Path
  ( Parser (..),
    Path,
    parse,
    match,
    part,
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

part :: Parser m => Text.Text -> m ()
part text = do
  text' <- param
  Monad.guard (text == text')

-- | Parses and discards a single parse segment matching the given @Text@ value
param :: (Web.FromHttpApiData a, Parser m) => m a
param = do
  maybePathSeg <- gets safeHead
  case maybePathSeg of
    Nothing -> do
      Error.next
    Just pathSeg -> do
      modify $ Prelude.drop 1
      case Web.parseUrlPieceMaybe pathSeg of
        Nothing -> do
          Error.next
        Just value -> do
          pure value
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Similar to `end` function in <https://github.com/purescript-contrib/purescript-routing/blob/main/GUIDE.md>
end :: Parser m => m ()
end = do
  currentPath <- parse
  if List.null currentPath
    then do
      pure ()
    else do
      Error.next
