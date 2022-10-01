{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Type.Response where

import Control.Applicative
import Control.Monad.Combinators
import Data.Aeson
import Data.Int
import Data.Text
import Data.Time
import GHC.Generics

data User = User
  { userID :: Int32,
    userEmail :: Text,
    -- userToken :: Text,
    userUsername :: Text,
    userBio :: Text,
    userImage :: Text,
    userCreatedAt :: UTCTime,
    userUpdatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

data EncodedUser = EncodedUser
  { encodedUserEmail :: Text,
    encodedUserToken :: Text,
    encodedUserUsername :: Text,
    encodedUserBio :: Text,
    encodedUserImage :: Text,
    encodedUserCreatedAt :: UTCTime,
    encodedUserUpdatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

{-
{
  "user": {
    "email": "jake@jake.jake",
    "token": "jwt.token.here",
    "username": "jake",
    "bio": "I work at statefarm",
    "image": null
  }
}
-}

instance ToJSON EncodedUser where
  toJSON EncodedUser {..} =
    object
      [ "user"
          .= object
            [ "email" .= encodedUserEmail,
              "token" .= encodedUserToken,
              "username" .= encodedUserUsername,
              "bio" .= encodedUserBio,
              "image" .= encodedUserImage
            ]
      ]

{-
{
  "profile": {
    "username": "jake",
    "bio": "I work at statefarm",
    "image": "https://api.realworld.io/images/smiley-cyrus.jpg",
    "following": false
  }
}
-}

data Profile = Profile
  { profileUsername :: Text,
    profileBio :: Text,
    profileImage :: Text,
    profileFollowing :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Profile where
  toJSON Profile {..} =
    object
      [ "profile"
          .= object
            [ "username" .= profileUsername,
              "bio" .= profileBio,
              "image" .= profileImage,
              "following" .= profileFollowing
            ]
      ]

{-
{
  "article": {
    "slug": "how-to-train-your-dragon",
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "It takes a Jacobian",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }
}
-}

data Author = Author
  { authorUsername :: Text,
    authorBio :: Text,
    authorImage :: Text,
    authorFollowing :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Author where
  toJSON Author {..} =
    object
      [ "username" .= authorUsername,
        "bio" .= authorBio,
        "image" .= authorImage,
        "following" .= authorFollowing
      ]

data Article = Article
  { articleSlug :: Text,
    articleTitle :: Text,
    articleDescription :: Text,
    articleBody :: Text,
    articleTagList :: [Text],
    articleCreatedAt :: UTCTime,
    articleUpdatedAt :: UTCTime,
    articleFavorited :: Bool,
    articleFavoritesCount :: Int32,
    articleAuthor :: Author
  }
  deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON Article {..} =
    object
      [ "article"
          .= object
            [ "slug" .= articleSlug,
              "title" .= articleTitle,
              "description" .= articleDescription,
              "body" .= articleBody,
              "tagList" .= articleTagList,
              "createdAt" .= articleCreatedAt,
              "updatedAt" .= articleUpdatedAt,
              "favorited" .= articleFavorited,
              "favoritesCount" .= articleFavoritesCount,
              "author" .= articleAuthor
            ]
      ]

{-
{
  "articles":[{
    "slug": "how-to-train-your-dragon",
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "It takes a Jacobian",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }, {
    "slug": "how-to-train-your-dragon-2",
    "title": "How to train your dragon 2",
    "description": "So toothless",
    "body": "It a dragon",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }],
  "articlesCount": 2
}
-}

data Articles = Articles
  { articlesArticleList :: [Article],
    articlesArticlesCount :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON Articles where
  toJSON Articles {..} =
    object
      [ "articles"
          .= fmap
            ( \Article {..} ->
                object
                  [ "slug" .= articleSlug,
                    "title" .= articleTitle,
                    "description" .= articleDescription,
                    "body" .= articleBody,
                    "tagList" .= articleTagList,
                    "createdAt" .= articleCreatedAt,
                    "updatedAt" .= articleUpdatedAt,
                    "favorited" .= articleFavorited,
                    "favoritesCount" .= articleFavoritesCount,
                    "author" .= articleAuthor
                  ]
            )
            articlesArticleList,
        "articlesCount" .= articlesArticlesCount
      ]

{-
{
  "comment": {
    "id": 1,
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:22:56.637Z",
    "body": "It takes a Jacobian",
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }
}
-}

data Comment = Comment
  { commentID :: Int32,
    commentCreatedAt :: UTCTime,
    commentUpdatedAt :: UTCTime,
    commentBody :: Text,
    commentAuthor :: Author
  }
  deriving (Eq, Show, Generic)

instance ToJSON Comment where
  toJSON Comment {..} =
    object
      [ "comment"
          .= object
            [ "id" .= commentID,
              "createdAt" .= commentCreatedAt,
              "updatedAt" .= commentUpdatedAt,
              "body" .= commentBody,
              "author" .= commentAuthor
            ]
      ]

newtype Comments = Comments
  {commentsCommentList :: [Comment]}
  deriving (Eq, Show, Generic)

instance ToJSON Comments where
  toJSON Comments {..} =
    object
      [ "comments"
          .= fmap
            ( \Comment {..} ->
                object
                  [ "id" .= commentID,
                    "createdAt" .= commentCreatedAt,
                    "updatedAt" .= commentUpdatedAt,
                    "body" .= commentBody,
                    "author" .= commentAuthor
                  ]
            )
            commentsCommentList
      ]

newtype Tags = Tags
  {tagsTagList :: [Text]}
  deriving (Eq, Show, Generic)

instance ToJSON Tags where
  toJSON Tags {..} =
    object
      [ "tags" .= tagsTagList
      ]

{-
{
  "errors":{
    "body": [
      "can't be empty"
    ]
  }
}
-}

newtype GenericError = GenericError
  { genericErrorErrors :: [Text]
  }
  deriving (Eq, Show, Generic)

genericError = encode $ GenericError ["Something happened...", "Try again."]

instance ToJSON GenericError where
  toJSON GenericError {..} =
    object
      [ "errors"
          .= object
            [ "body" .= genericErrorErrors
            ]
      ]
