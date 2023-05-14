{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}

module Data where

import Crypto.JWT (SignedJWT)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Okapi.Parser.Body as Body
import Web.HttpApiData (FromHttpApiData)

data UserLogin = UserLogin
  deriving (Eq, Show, Generic, FromJSON)

instance Body.Interface UserLogin where
  body = Body.json :| []

data UserRegistration = UserRegistration
  deriving (Eq, Show, Generic, FromJSON)

data UserUpdate = UserUpdate
  deriving (Eq, Show, Generic, FromJSON)

data Profile = Profile
  deriving (Eq, Show, Generic, ToJSON)

data User = User
  { email :: Text,
    token :: SignedJWT,
    username :: Username,
    bio :: Text,
    image :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

newtype Tag = Tag {text :: Text}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema, ToJSON, FromJSON)

newtype Username = Username {text :: Text}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema, ToJSON, FromJSON)

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

data FeedQuery = FeedQuery
  { limit :: Limit,
    offset :: Offset
  }
  deriving (Eq, Show)

newtype Slug = Slug {text :: Text}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema)

data Article = Article
  deriving (Eq, Show, Generic, ToJSON)

data NewArticle = NewArticle
  { title :: Text,
    description :: Text,
    body :: Text,
    tagList :: Maybe [Tag]
  }
  deriving (Generic, FromJSON)

data ArticleUpdate = ArticleUpdate
  { title :: Maybe Text,
    description :: Maybe Text,
    body :: Maybe Text
  }
  deriving (Generic, FromJSON)

data NewComment = NewComment
  { body :: Text
  }
  deriving (Generic, FromJSON)

data Comment = Comment
  deriving (Eq, Show, Generic, ToJSON)

newtype ID a = ID {int :: Int}
  deriving newtype (Eq, Show, FromHttpApiData, ToSchema)