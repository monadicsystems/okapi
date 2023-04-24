{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import API (buildWithContext)
import API.AddComment qualified as AddComent
import API.AddComment qualified as AddComment
import App (Config)
import Data.Text qualified as Text
import Database
import Okapi.Endpoint
import Okapi.Script.AddHeader qualified as AddHeader
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Text.Pretty.Simple (pPrint)
import Web.HttpApiData qualified as Web

getConfig :: IO Config
getConfig = undefined

main :: IO ()
main = do
  config <- getConfig
  let builder = buildWithContext config
  pure ()

realworldAPI builder =
  Server
    { info = mempty,
      url = ["api"],
      description = Nothing,
      -- defaultResponse = mempty,
      artifacts =
        [ builder AddComent.endpoint AddComment.handler,
          undefined
        ]
    }
