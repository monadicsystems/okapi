{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Plan.UnFollowUser where

import Data
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

plan =
  Plan
    { transformer = id,
      endpoint =
        Endpoint
          { method = DELETE,
            path = do
              Path.static "profiles"
              username <- Path.param @Username "username"
              Path.static "unfollow"
              pure username,
            query = pure (),
            body = pure (),
            headers = pure (),
            responder = Responder.json @Profile status200 $ pure ()
          },
      handler = \username _ _ _ responder -> do
        print username
        return $ responder (\() response -> response) Profile
    }
