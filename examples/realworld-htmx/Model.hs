module Model where

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Conduit.Model where

import qualified Data.Aeson as Aeson
import Data.Int (Int32)
import Data.Text
import Data.Time
import GHC.Generics
import Servant.API (FromHttpApiData, FormUrlEncoded, ToHttpApiData)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Web.FormUrlEncoded (FromForm)

newtype ID a = ID {unID :: Int32}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
  deriving newtype (ToHttpApiData, FromHttpApiData)

-- CORE MODELS START --

data User = User
  { userBio :: Text,
    userEmail :: Text,
    userID :: ID User,
    userImageUrl :: Text,
    userUsername :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, FromJWT, ToJSON, ToJWT)

data Article = Article
  { articleAuthorID :: ID User,
    articleBody :: Text,
    articleCreatedAt :: UTCTime,
    articleDescription :: Text,
    articleFavorites :: Int32,
    articleID :: ID Article,
    articleTitle :: Text
  }
  deriving (Eq, Show)

newtype Tag = Tag
  { unTag :: Text
  }
  deriving (Eq, Show)

data Comment = Comment
  { commentArticleID :: ID Article,
    commentAuthorID :: ID User,
    commentBody :: Text,
    commentCreatedAt :: UTCTime,
    commentID :: ID Comment
  }
  deriving (Eq, Show)

data Follow = (ID User) :-> (ID User)

-- CORE MODELS END --

-- FORM MODELS START --

data LoginForm = LoginForm
  { loginFormEmail :: Text,
    loginFormPassword :: Text
  }
  deriving (FromForm, Generic, Show)

instance ToJSON LoginForm where
  toJSON (LoginForm email password) =
    object
      [ "email" .= email,
        "password" .= password
      ]

data RegisterForm = RegisterForm
  { registerFormEmail :: Text,
    registerFormPassword :: Text,
    registerFormUsername :: Text
  }
  deriving (FromForm, Generic, Show)

instance ToJSON RegisterForm where
  toJSON (RegisterForm email password username) =
    object
      [ "email" .= email,
        "password" .= password,
        "username" .= username
      ]

data SettingsForm = SettingsForm
  { settingsFormBio :: Text,
    settingsFormEmail :: Text,
    settingsFormImageUrl :: Text,
    settingsFormNewPassword :: Text,
    settingsFormUserID :: ID User,
    settingsFormUsername :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data PublishForm = PublishForm
  { publishFormBody :: Text,
    publishFormDescription :: Text,
    publishFormTags :: Text,
    publishFormTitle :: Text
  }
  deriving (Generic, Eq, Show, FromForm)

instance ToJSON PublishForm where
  toJSON PublishForm{..} =
    object
      [ "body" .= publishFormBody,
        "description" .= publishFormDescription,
        "tags" .= publishFormTags,
        "title" .= publishFormTitle
      ]

data EditArticleForm = EditArticleForm
  { editArticleFormArticleID :: ID Article,
    editArticleFormBody :: Text,
    editArticleFormDescription :: Text,
    editArticleFormTags :: Text,
    editArticleFormTitle :: Text
  }
  deriving (Generic, FromForm, Eq, Show)

instance ToJSON EditArticleForm where
  toJSON EditArticleForm{..} =
    object
      [ "articleID" .= editArticleFormArticleID,
        "body" .= editArticleFormBody,
        "description" .= editArticleFormDescription,
        "tags" .= editArticleFormTags,
        "title" .= editArticleFormTitle
      ]

data CommentForm = CommentForm
  { commentFormArticleID :: ID Article,
    commentFormAuthorID :: ID User,
    commentFormBody :: Text
  }
  deriving (Eq, Show)

newtype FollowForm = FollowForm
  { followFormTarget :: Text
  }
  deriving (FromForm, Generic, Show)

newtype UnfollowForm = UnfollowForm
  { unfollowFormTarget :: Text
  }
  deriving (FromForm, Generic, Show)

type ArticleInfo =
  ( Article
  , User
  , [Tag]
  )

-- FORM MODELS END --
