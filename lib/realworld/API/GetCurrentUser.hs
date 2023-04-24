{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module API.GetCurrentUser where

import API (auth)
import Data (User (..))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Okapi.Endpoint
import Okapi.Script.AddHeader (Response)
import qualified Okapi.Script.AddHeader as AddHeader
import qualified Okapi.Script.Body as Body
import qualified Okapi.Script.Headers as Headers
import qualified Okapi.Script.Path as Path
import qualified Okapi.Script.Query as Query
import qualified Okapi.Script.Responder as Responder

plan =
  Plan
    { transformer = id,
      endpoint =
        Endpoint
          { security = auth,
            method = GET,
            path = Path.static "user",
            query = pure (),
            body = pure (),
            headers = pure (),
            responder = Responder.json @User status200 $ pure ()
          },
      handler = \token _ _ _ userRegistration responder -> do
        print userRegistration
        return $ responder (\() response -> response) undefined
    }
