{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Route where

import qualified Data
import qualified Okapi.Spec.Request.Route as Route

data Login = Login

instance Route.Interface Login where
  route = do
    Route.static "users"
    Route.static "login"
    pure Login

data Users = Users

data User = User

data Profile = Profile Data.Username

data Follow = Follow Data.Username

data Feed = Feed

data Articles = Articles

data Article = Article Data.Slug

data Comments = Comments Data.Slug

data Comment = Comment Data.Slug (Data.ID Data.Comment)

data Favorite = Favorite Data.Slug

data Tags = Tags
