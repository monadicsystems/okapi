{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE TypeApplications #-}

module Main where

import API (buildWith)
import API.AddComment qualified as AddComment
import API.Authentication qualified as Authentication
import API.CreateArticle qualified as CreateArticle
import API.DeleteArticle qualified as DeleteArticle
import API.DeleteComment qualified as DeleteComment
import API.FavoriteArticle qualified as FavoriteArticle
import API.FeedArticles qualified as FeedArticles
import API.FollowUser qualified as FollowUser
import API.GetArticle qualified as GetArticle
import API.GetComments qualified as GetComments
import API.GetCurrentUser qualified as GetCurrentUser
import API.GetProfile qualified as GetProfile
import API.GetTags qualified as GetTags
import API.ListArticles qualified as ListArticles
import API.Registration qualified as Registration
import API.UnFavoriteArticle qualified as UnFavoriteArticle
import API.UnFollowUser qualified as UnFollowUser
import API.UpdateArticle qualified as UpdateArticle
import API.UpdateUser qualified as UpdateUser
import App (Config (..), Context)
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Database
import Database.Redis qualified as Redis
import Database.SQLite.Simple qualified as Sqlite
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp qualified as Warp
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
getConfig = do
  redisConn <- Redis.checkedConnect Redis.defaultConnectInfo
  sqliteConn <- Sqlite.open "realworld/realworld.db"
  return Config {..}

main :: IO ()
main = do
  config <- getConfig
  let api = realworldAPI config
  -- TODO: Linearity constraint doesn't get passed to builder?? Linear types BUG
  LBS.writeFile "genopenapi.yml" $ Aeson.encodePretty $ toOpenApi api
  -- Warp.run 3000 $ toApp api
  pure ()

realworldAPI config =
  Server
    { info = mempty,
      url = ["api"],
      description = Nothing,
      defaultResponse = WAI.responseLBS HTTP.status404 [] "",
      artifacts =
        [ buildWith config Authentication.plan,
          buildWith config AddComment.plan,
          buildWith config CreateArticle.plan,
          buildWith config DeleteArticle.plan,
          buildWith config DeleteComment.plan,
          buildWith config FavoriteArticle.plan,
          buildWith config FeedArticles.plan,
          buildWith config FollowUser.plan,
          buildWith config GetArticle.plan,
          buildWith config GetComments.plan,
          buildWith config GetCurrentUser.plan,
          buildWith config GetProfile.plan,
          buildWith config GetTags.plan,
          buildWith config ListArticles.plan,
          buildWith config Registration.plan,
          buildWith config UnFavoriteArticle.plan,
          buildWith config UnFollowUser.plan,
          buildWith config UpdateArticle.plan,
          buildWith config UpdateUser.plan
        ]
    }
