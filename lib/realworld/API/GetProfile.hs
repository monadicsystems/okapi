{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module API.GetProfile where

import API (auth)
import Control.Applicative ((<|>))
import Data (User (..), Username)
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
    { security = (Right <$> auth) <|> (Left <$> Security.none),
      method = GET,
      path = Path.static "profiles" *> Path.param @Username "username",
      query = pure (),
      body = pure (),
      headers = pure (),
      responder = Responder.json @User status200 $ pure ()
    }

handler' :: Monad m => p1 -> p2 -> p3 -> p4 -> p5 -> ((() %1 -> p6 -> p6) -> t -> a) -> m a
handler' tokenOrNone username _ _ _ responder = do
  -- print username
  return $ responder (\() response -> response) undefined
