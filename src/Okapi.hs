{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi
  ( -- TOP LEVEL
    run,
    runTLS,
    runWebsockets,
    runWebsocketsTLS,
    -- APPLICATION
    module Okapi.Application,
    -- SERVER SENT EVENTS
    newEventSource,
    sendValue,
    sendEvent,
    -- PARSER
    module Okapi.Parser,
    -- RESPONSE
    module Okapi.Response,
    -- ROUTE
    module Okapi.Route,
    -- TEST
    module Okapi.Test,
    -- TYPES
    MonadOkapi,
    OkapiT,
    State,
    Request,
    Response,
    ResponseBody,
    Failure,
    Path,
    Headers,
    QueryItem,
    Query,
    Cookie,
    Cookies,
    ToSSE (..),
    Event (..),
    Chan,
    EventSource,
    URL (..),
  )
where

import Control.Applicative.Combinators
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
import qualified Network.HTTP.Types as HTTP
import Network.Wai (ResponseReceived)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Internal as Wai
import Network.Wai.Middleware.Gzip (def, gzip)
import qualified Network.WebSockets as WS
import Okapi.Application
import Okapi.Event
import qualified Okapi.Event as Event
import Okapi.Parser
import Okapi.Response
import Okapi.Route
import Okapi.Test
import Okapi.Types
import qualified Web.Cookie as Cookie
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

run :: Monad m => (forall a. m a -> IO a) -> Response -> Int -> OkapiT m Response -> IO ()
run hoister defaultResponse port okapiT = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ app hoister defaultResponse okapiT

runTLS :: Monad m => (forall a. m a -> IO a) -> Response -> Warp.TLSSettings -> Warp.Settings -> OkapiT m Response -> IO ()
runTLS hoister defaultResponse tlsSettings settings okapiT = do
  print "Running servo on port 43"
  Warp.runTLS tlsSettings settings $ app hoister defaultResponse okapiT

runWebsockets :: Monad m => (forall a. m a -> IO a) -> Response -> Int -> OkapiT m Response -> WS.ConnectionOptions -> WS.ServerApp -> IO ()
runWebsockets hoister defaultResponse port okapiT connSettings serverApp = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ websocketsApp hoister defaultResponse okapiT connSettings serverApp

runWebsocketsTLS :: Monad m => (forall a. m a -> IO a) -> Response -> Warp.TLSSettings -> Warp.Settings -> OkapiT m Response -> WS.ConnectionOptions -> WS.ServerApp -> IO ()
runWebsocketsTLS hoister defaultResponse tlsSettings settings okapiT connSettings serverApp = do
  Warp.runTLS tlsSettings settings $ websocketsApp hoister defaultResponse okapiT connSettings serverApp
