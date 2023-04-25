{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module API.UnFollowUser where

import API (auth)
import Data (Article (..), Slug, User (..), Username)
import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OAPI
import Data.Text (Text)
import GHC.Generics (Generic)
import Okapi.Endpoint
import Okapi.Script.AddHeader (Response)
import qualified Okapi.Script.AddHeader as AddHeader
import qualified Okapi.Script.Body as Body
import qualified Okapi.Script.Headers as Headers
import qualified Okapi.Script.Path as Path
import qualified Okapi.Script.Query as Query
import qualified Okapi.Script.Responder as Responder
import qualified Web.HttpApiData as Web

plan = Plan endpoint' handler'

endpoint' =
  Endpoint
    { security = auth,
      method = DELETE,
      path = do
        Path.static "articles"
        slug <- Path.param @Slug "slug"
        Path.static "favorite"
        pure slug,
      query = pure (),
      body = pure (),
      headers = pure (),
      responder = Responder.json @Article status200 $ pure ()
    }

handler' :: Monad m => p1 -> p2 -> p3 -> p4 -> p5 -> ((() %1 -> p6 -> p6) -> Article -> a) -> m a
handler' token username _ _ _ responder = do
  -- print username
  return $ responder (\() response -> response) Article
