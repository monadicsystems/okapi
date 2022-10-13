{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Int (Int32)
import Data.Text
import Data.Time
import GHC.Generics
import Web.HttpApiData
import Web.FormUrlEncoded (FromForm)

newtype ID a = ID {unID :: Int32}
  deriving (Eq, Show, Generic)
  deriving newtype (ToHttpApiData, FromHttpApiData)

-- CORE MODELS START --

data User = User
  { userBio :: Text,
    userEmail :: Text,
    userID :: ID User,
    userImageUrl :: Text,
    userUsername :: Text
  }
  deriving (Eq, Show, Generic)

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

data RegisterForm = RegisterForm
  { registerFormEmail :: Text,
    registerFormPassword :: Text,
    registerFormUsername :: Text
  }
  deriving (FromForm, Generic, Show)

data SettingsForm = SettingsForm
  { settingsFormBio :: Text,
    settingsFormEmail :: Text,
    settingsFormImageUrl :: Text,
    settingsFormNewPassword :: Text,
    settingsFormUserID :: ID User,
    settingsFormUsername :: Text
  }
  deriving (Eq, Show, Generic)

data PublishForm = PublishForm
  { publishFormBody :: Text,
    publishFormDescription :: Text,
    publishFormTags :: Text,
    publishFormTitle :: Text
  }
  deriving (Generic, Eq, Show, FromForm)

data EditArticleForm = EditArticleForm
  { editArticleFormArticleID :: ID Article,
    editArticleFormBody :: Text,
    editArticleFormDescription :: Text,
    editArticleFormTags :: Text,
    editArticleFormTitle :: Text
  }
  deriving (Generic, FromForm, Eq, Show)

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
