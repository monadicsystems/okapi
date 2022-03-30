{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Type.Request where

import Control.Applicative
import Control.Monad.Combinators
import Data.Aeson
import Data.Int
import Data.Text
import Data.Time
import GHC.Generics

data Login = Login
  { loginEmail :: Text,
    loginPassword :: Text
  }

instance FromJSON Login where
  parseJSON = withObject "login" $ \o -> do
    userO <- o .: "user"
    loginEmail <- userO .: "email"
    loginPassword <- userO .: "password"
    pure Login {..}

data Register = Register
  { registerEmail :: Text,
    registerUsername :: Text,
    registerPassword :: Text
  }

instance FromJSON Register where
  parseJSON = withObject "register" $ \o -> do
    userO <- o .: "user"
    registerEmail <- userO .: "email"
    registerUsername <- userO .: "username"
    registerPassword <- userO .: "password"
    pure Register {..}

data UpdateUser = UpdateUser
  { updateUserEmail :: Maybe Text,
    updateUserUsername :: Maybe Text,
    updateUserPassword :: Maybe Text,
    updateUserBio :: Maybe Text,
    updateUserImage :: Maybe Text
  }

instance FromJSON UpdateUser where
  parseJSON = withObject "updateUser" $ \o -> do
    userO <- o .: "user"
    updateUserEmail <- userO .:? "email"
    updateUserUsername <- userO .:? "username"
    updateUserPassword <- userO .:? "password"
    updateUserImage <- userO .:? "image"
    updateUserBio <- userO .:? "bio"
    pure UpdateUser {..}

data ArticlesQuery = ArticlesQuery
  { articlesQueryTag :: Maybe Text,
    articlesQueryAuthor :: Maybe Text,
    articlesQueryFavorited :: Maybe Text,
    articlesQueryLimit :: Int32,
    articlesQueryOffset :: Int32
  }

data CreateArticle = CreateArticle
  { createArticleTitle :: Text,
    createArticleDescription :: Text,
    createArticleBody :: Text,
    createArticleTagList :: Maybe [Text]
  }

instance FromJSON CreateArticle where
  parseJSON = withObject "createArticle" $ \o -> do
    articleO <- o .: "article"
    createArticleTitle <- articleO .: "title"
    createArticleDescription <- articleO .: "description"
    createArticleBody <- articleO .: "body"
    createArticleTagList <- articleO .:? "tagList"
    pure CreateArticle {..}

data UpdateArticle = UpdateArticle
  { updateArticleTitle :: Maybe Text,
    updateArticleDescription :: Maybe Text,
    updateArticleBody :: Maybe Text
  }

instance FromJSON UpdateArticle where
  parseJSON = withObject "updateArticle" $ \o -> do
    articleO <- o .: "article"
    updateArticleTitle <- articleO .:? "title"
    updateArticleDescription <- articleO .:? "description"
    updateArticleBody <- articleO .:? "body"
    pure UpdateArticle {..}

newtype CreateComment = CreateComment
  { createCommentBody :: Text
  }

instance FromJSON CreateComment where
  parseJSON = withObject "createComment" $ \o -> do
    commentO <- o .: "comment"
    createCommentBody <- commentO .: "body"
    pure CreateComment {..}
