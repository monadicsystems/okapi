{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}

module Data where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData)

data UserLogin = UserLogin
  deriving (Eq, Show, Generic, FromJSON)

data UserRegistration = UserRegistration
  deriving (Eq, Show, Generic, FromJSON)

data UserUpdate = UserUpdate
  deriving (Eq, Show, Generic, FromJSON)

data Profile = Profile
  deriving (Eq, Show, Generic, ToJSON)

data User = User
  deriving (Eq, Show, Generic, ToJSON)

newtype Tag = Tag {text :: Text}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema, ToJSON)

newtype Username = Username {text :: Text}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema, ToJSON)

newtype Limit = Limit {int :: Int}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema, ToJSON)

newtype Offset = Offset {int :: Int}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema, ToJSON)

data ArticlesQuery = ArticlesQuery
  { tag :: Maybe Tag,
    author :: Maybe Username,
    favorited :: Maybe Username,
    limit :: Limit,
    offset :: Offset
  }
  deriving (Eq, Show)
