module Resource where

import qualified Data

data Login = Login

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
