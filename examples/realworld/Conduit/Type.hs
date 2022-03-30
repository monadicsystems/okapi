{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Type
  ( module Conduit.Type.Request,
    module Conduit.Type.Response,
    Handler (..),
    Config (..),
    MonadHandler,
    grab,
    Has (..),
  )
where

import Conduit.Type.Request
import Conduit.Type.Response
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text
import Hasql.Connection (Connection)

data Config = Config
  { configJWTSecret :: Text,
    configDBConnection :: Connection
  }

class Has field env where
  obtain :: env -> field

instance Has Connection Config where
  obtain = configDBConnection

instance Has Text Config where
  obtain = configJWTSecret

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field

newtype Handler a = Handler {runHandler :: ReaderT Config IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadIO
    )

type MonadHandler m =
  ( Monad m,
    MonadReader Config m,
    MonadIO m
  )
