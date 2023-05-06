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
{-# LANGUAGE TypeApplications #-}

module Main where

import App (Config (..), Context)
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
import Okapi.Script.Body qualified as Body
import Okapi.Script.Headers qualified as Headers
import Okapi.Script.Path qualified as Path
import Okapi.Script.Query qualified as Query
import Okapi.Script.Responder qualified as Responder
import Okapi.Script.Responder.AddHeader qualified as AddHeader
import Okapi.Script.Security qualified as Security
import Okapi.Script.Security.Secure qualified as Secure
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

auth :: Secure.Script Token
auth = Secure.apiKey @Token Secure.Header "Authorization"

type Resources =
  '[ Resource.Login,
     Resource.Users,
     Resource.User,
     Resource.Profile,
     Resource.Follow,
     Resource.Feed,
     Resource.Articles,
     Resource.Article,
     Resource.Comments,
     Resource.Comment,
     Resource.Favorite,
     Resource.Tags
   ]

main :: IO ()
main = do
  config <- getConfig
  let object = Object (\context -> runReaderT context config)
  let server = toServer object

  LBS.writeFile "genopenapi.yml" $ Aeson.encodePretty $ toOpenAPI server
  Warp.run 3000 $ toApplication server
  pure ()

toServer :: Object Context -> Server Resources
toServer object =
  Server
    { info = mempty,
      url = ["api"],
      description = Nothing,
      pathItems =
        [ toLogin object,
          toUsers object,
          toUser object,
          toProfile object,
          toFollow object,
          toFeed object,
          toArticles object,
          toArticle object,
          toComments object,
          toComment object,
          toFavorite object,
          toTags object
        ]
    }

toLogin :: Object Context -> PathItem Resource.Login
toLogin object =
  PathItem
    { summary = Nothing,
      description = Nothing,
      path = do
        Path.static "users"
        Path.static "login"
        pure Resource.Login,
      get =
        Just $
          POST
            { summary = Nothing,
              description = Nothing,
              security = Security.None :| [],
              query = pure (),
              headers = pure (),
              body = Body.None :| [],
              responder = pure (),
              handler = undefined,
              object = object
            },
      post = Nothing,
      put = Nothing,
      delete = Nothing
    }
