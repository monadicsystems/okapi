{-# LANGUAGE OverloadedStrings #-}

module Okapi.Log (logIt) where

import qualified Control.Monad.Logger as Logger
import qualified Data.Text as Text

logIt :: (Show a, Logger.MonadLogger m) => a -> m ()
logIt = Logger.logOtherN (Logger.LevelOther "OKAPI") . toText
  where
    toText :: Show a => a -> Text.Text
    toText = Text.pack . show