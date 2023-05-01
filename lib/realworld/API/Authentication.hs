{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module API.Authentication where

import Data (User (..), UserLogin)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Okapi.Operation
import Okapi.Script.AddHeader (Response)
import qualified Okapi.Script.AddHeader as AddHeader
import qualified Okapi.Script.Body as Body
import qualified Okapi.Script.Headers as Headers
import qualified Okapi.Script.Path as Path
import qualified Okapi.Script.Query as Query
import qualified Okapi.Script.Responder as Responder
import qualified Okapi.Script.Security as Security

plan = Plan endpoint' handler'

endpoint' =
  Operation
    { security = Security.none,
      method = POST,
      path = Path.static "users" *> Path.static "login",
      query = pure (),
      body = Body.json @UserLogin,
      headers = pure (),
      responder = Responder.json @User status200 $ pure ()
    }

handler' :: Monad m => () -> p1 -> p2 -> p3 -> p4 -> ((() %1 -> p5 -> p5) -> t -> a) -> m a
handler' () _ _ _ userLogin responder = do
  -- print userLogin
  return $ responder (\() response -> response) undefined

-- TODO: Needs this if Plan parts are inside a where clause for some reason
-- addHeader :: () %1 -> Response -> Response
-- addHeader () = id
