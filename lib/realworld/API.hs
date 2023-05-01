{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API where

import App (Config, Context)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Natural
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.OpenApi qualified as OAPI
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Okapi.Operation (Artifact, Handler, Operation, Plan, build)
import Okapi.Script.Security
import Okapi.Script.Security qualified as Security
import Web.HttpApiData qualified as Web

newtype Token = Token {text :: Text.Text}
  deriving newtype (Eq, Show, Web.FromHttpApiData, OAPI.ToSchema, Aeson.ToJSON)

auth :: Security.Script Token
auth = Security.apiKey @Token Header "Authorization"

buildWith ::
  forall s p q h b r.
  Config ->
  Plan Context s p q h b r ->
  Artifact
buildWith config = build (transformer config)
  where
    transformer :: Config -> Context ~> IO
    transformer config context = runReaderT context config
