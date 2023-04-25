{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module API.GetTags where

import Data (ArticlesQuery (..), Limit (..), Offset (..), Tag, User (..), Username)
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
import qualified Okapi.Script.Security as Security
import qualified Web.HttpApiData as Web

plan = Plan endpoint' handler'

endpoint' =
  Endpoint
    { security = Security.none,
      method = GET,
      path = Path.static "tags",
      query = pure (),
      body = pure (),
      headers = pure (),
      responder = Responder.json @[Tag] status200 $ pure ()
    }

handler' :: Monad m => () -> p1 -> p2 -> p3 -> p4 -> ((() %1 -> p5 -> p5) -> [a1] -> a2) -> m a2
handler' () _ _ _ userRegistration responder = do
  -- print userRegistration
  return $ responder (\() response -> response) []
