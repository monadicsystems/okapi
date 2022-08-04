{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi
  ( -- MODULES
    module Okapi.Synonym,
    module Okapi.Parser,
    module Okapi.Response,
    -- TYPES
    State,
    Request,
    ToSSE (..),
    Event (..),
    EventSource,
    -- FOR RUNNING OKAPI
    runOkapi,
    runOkapiTLS,
    makeOkapiApp,
    -- METHOD HELPERS
    method,
    get,
    post,
    head,
    put,
    delete,
    trace,
    connect,
    options,
    patch,
    anyMethod,
    -- PATH HELPERS
    pathSegWith,
    pathSeg,
    path,
    pathParam,
    pathParamRaw,
    -- QUERY HELPERS
    queryParam,
    queryParamRaw,
    queryFlag,
    -- HEADER HELPERS
    header,
    basicAuth,
    cookies,
    -- BODY HELPERS
    bodyRaw,
    bodyJSON,
    bodyForm,
    -- SERVER SIDE EVENT HELPERS
    newEventSource,
    sendEvent,
    sendValue,
    -- RESPONSE HELPERS
    respond,
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Monad (MonadPlus, guard, (>=>))
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Base64
import qualified GHC.Natural as Natural
import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import Network.Wai (ResponseReceived)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Internal as Wai
import Network.Wai.Middleware.Gzip (def, gzip)
import Okapi.Event
import qualified Okapi.Event as Event
import Okapi.Parser
import Okapi.Response
import Okapi.State
import qualified Web.Cookie as Cookie
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (error, head)
import Control.Applicative.Combinators

-- FOR RUNNING OKAPI

runOkapi :: Monad m => (forall a. m a -> IO a) -> Response -> Int -> OkapiT m Response -> IO ()
runOkapi hoister defaultResponse port okapiT = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ makeOkapiApp hoister defaultResponse okapiT

runOkapiTLS :: Monad m => (forall a. m a -> IO a) -> Response -> Warp.TLSSettings -> Warp.Settings -> OkapiT m Response -> IO ()
runOkapiTLS hoister defaultResponse tlsSettings settings okapiT = do
  print "Running servo on port 43"
  Warp.runTLS tlsSettings settings $ makeOkapiApp hoister defaultResponse okapiT
