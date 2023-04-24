{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module API where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Okapi.Script.Security
import Okapi.Script.Security qualified as Security
import Web.HttpApiData qualified as Web

newtype Token = Token {bytes :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

auth :: Security.Script Token
auth = Security.apiKey @Token Header "Authorization"
