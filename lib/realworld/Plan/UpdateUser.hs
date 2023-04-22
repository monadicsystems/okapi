{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Plan.UpdateUser where

import Data (User (..), UserUpdate)
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
          { method = GET,
            path = Path.static "user",
            query = pure (),
            body = Body.json @UserUpdate,
            headers = pure (),
            responder = Responder.json @User status200 $ pure ()
          },
      handler = \_ _ _ userRegistration responder -> do
        print userRegistration
        return $ responder (\() response -> response) User
    }
