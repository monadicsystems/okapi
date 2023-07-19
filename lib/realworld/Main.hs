{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import API (toAPI)
import App (Config (..), Context)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Object (Object (..))
import Data
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import Database
import Database.Redis qualified as Redis
import Database.SQLite.Simple qualified as Sqlite
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.Operation
import Okapi.Spec.Request.Body qualified as Body
import Okapi.Spec.Request.Headers qualified as Headers
import Okapi.Spec.Request.Query qualified as Query
import Okapi.Spec.Response qualified as Response
import Okapi.Spec.Response.Headers qualified as Headers
import Okapi.Spec.Request.Route qualified as Route
import Okapi.Spec.Request.Security qualified as Security
-- import Okapi.Spec.Request.Security.Secure qualified as Secure
import Resource qualified
import Text.Pretty.Simple (pPrint)
import Web.HttpApiData qualified as Web

getConfig :: IO Config
getConfig = do
  redisConn <- Redis.checkedConnect Redis.defaultConnectInfo
  sqliteConn <- Sqlite.open "realworld/realworld.db"
  return Config {..}

newtype Token = Token {text :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

auth :: Secure.Spec Token
auth = Secure.apiKey @Token Secure.Headers "Authorization"

main :: IO ()
main = do
  config <- getConfig
  let object = Object (`runReaderT` config)
  let server = toAPI object

  LBS.writeFile "genopenapi.yml" $ Aeson.encodePretty $ toOpenAPI server
  -- Warp.run 3000 $ toApplication server
  print "Generated OAPI"
  pure ()
