{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Effect.Request where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Vault.Lazy as Vault
import qualified Network.Server.Types as Server
import qualified Network.Wai.Parse as WAI
import qualified Okapi.Effect.Failure as Failure
-- import qualified Okapi.Pattern as Pattern

import qualified Okapi.Effect.Log as Log
import qualified Okapi.Effect.Request.Body as Body
import qualified Okapi.Effect.Request.Headers as Headers
import qualified Okapi.Effect.Request.Method as Method
import qualified Okapi.Effect.Request.Path as Path
import qualified Okapi.Effect.Request.Query as Query
import qualified Okapi.Effect.Request.Vault as Vault
import qualified Okapi.State.Request as Request
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

type MonadRequest m = (Method.MonadMethod m, Path.MonadPath m, Query.MonadQuery m, Headers.MonadHeaders m, Body.MonadBody m, Vault.MonadVault m)

-- | Parses the entire request.
request :: MonadRequest m => m Request.Request
request = Request.Request <$> Method.method <*> Path.path <*> Query.query <*> Body.body <*> Headers.headers <*> Vault.vault

requestEnd :: MonadRequest m => m ()
requestEnd = do
  Method.methodEnd
  Path.pathEnd
  Query.queryEnd
  Headers.headersEnd
  Body.bodyEnd
