{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import API.Login
import App (Context)
import Control.Object (Object)
import Okapi.Operation (PathItems (..), Server (..))
import qualified Resource

type Resources =
  '[ Resource.Login
  -- ,
  --  Resource.Users,
  --  Resource.User,
  --  Resource.Profile,
  --  Resource.Follow,
  --  Resource.Feed,
  --  Resource.Articles,
  --  Resource.Article,
  --  Resource.Comments,
  --  Resource.Comment,
  --  Resource.Favorite,
  --  Resource.Tags
   ]

toAPI :: Object Context -> Server Resources
toAPI object =
  Server
    { info = mempty,
      url = ["api"],
      description = Nothing,
      pathItems = toLogin object :& Nil
    }
