{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Server where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Strict as State
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
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Parse as WAI
import qualified Okapi.Event as Event
import qualified Okapi.Request as Request
import qualified Okapi.Response as Response
import qualified Okapi.Server.Failure as Failure
import qualified Okapi.Server.Request as Request hiding (path)
import qualified Okapi.Server.Response as Response
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

-- | A concrete implementation of the @ServerM@ type constraint.
newtype ServerT m a = ServerT {runServerT :: Except.ExceptT Failure.Failure (State.StateT (Request.Request, Response.Response) m) a}
  deriving newtype
    ( Except.MonadError Failure.Failure
    )

instance Functor m => Functor (ServerT m) where
  fmap :: (a -> b) -> ServerT m a -> ServerT m b
  fmap f okapiT =
    ServerT . Except.ExceptT . State.StateT $
      fmap (\ ~(a, s') -> (f <$> a, s'))
        . State.runStateT (Except.runExceptT $ runServerT okapiT)
  {-# INLINE fmap #-}

instance Monad m => Applicative (ServerT m) where
  pure :: Monad m => a -> ServerT m a
  pure x = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
  (<*>) :: Monad m => ServerT m (a -> b) -> ServerT m a -> ServerT m b
  (ServerT (Except.ExceptT (State.StateT mf))) <*> (ServerT (Except.ExceptT (State.StateT mx))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherF, s') <- mf s
    case eitherF of
      Left error -> pure (Left error, s)
      Right f -> do
        ~(eitherX, s'') <- mx s'
        case eitherX of
          Left error' -> pure (Left error', s')
          Right x -> pure (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}
  (*>) :: Monad m => ServerT m a -> ServerT m b -> ServerT m b
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance Monad m => Applicative.Alternative (ServerT m) where
  empty :: Monad m => ServerT m a
  empty = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Left Failure.Next, s)
  {-# INLINE empty #-}
  (<|>) :: Monad m => ServerT m a -> ServerT m a -> ServerT m a
  (ServerT (Except.ExceptT (State.StateT mx))) <|> (ServerT (Except.ExceptT (State.StateT my))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Failure.Next -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Failure.Next -> pure (Left Failure.Next, s)
          Left error@(Failure.Abort _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Failure.Abort _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE (<|>) #-}

instance Monad m => Monad (ServerT m) where
  return :: Monad m => a -> ServerT m a
  return = pure
  {-# INLINEABLE return #-}
  (>>=) :: Monad m => ServerT m a -> (a -> ServerT m b) -> ServerT m b
  (ServerT (Except.ExceptT (State.StateT mx))) >>= f = ServerT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherX, s') <- mx s
    case eitherX of
      Left error -> pure (Left error, s)
      Right x -> do
        ~(eitherResult, s'') <- State.runStateT (Except.runExceptT $ runServerT $ f x) s'
        case eitherResult of
          Left error' -> pure (Left error', s')
          Right res -> pure (Right res, s'')
  {-# INLINEABLE (>>=) #-}

instance Monad m => Monad.MonadPlus (ServerT m) where
  mzero :: Monad m => ServerT m a
  mzero = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Left Failure.Next, s)
  {-# INLINE mzero #-}
  mplus :: Monad m => ServerT m a -> ServerT m a -> ServerT m a
  (ServerT (Except.ExceptT (State.StateT mx))) `mplus` (ServerT (Except.ExceptT (State.StateT my))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Failure.Next -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Failure.Next -> pure (Left Failure.Next, s)
          Left error@(Failure.Abort _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Failure.Abort _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance Reader.MonadReader r m => Reader.MonadReader r (ServerT m) where
  ask :: Reader.MonadReader r m => ServerT m r
  ask = Morph.lift Reader.ask
  local :: Reader.MonadReader r m => (r -> r) -> ServerT m a -> ServerT m a
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either Failure.Failure a, (Request.Request, Response.Response)) -> n (Either Failure.Failure b, (Request.Request, Response.Response))) -> ServerT m a -> ServerT n b
      mapOkapiT f okapiT = ServerT . Except.ExceptT . State.StateT $ f . State.runStateT (Except.runExceptT $ runServerT okapiT)
  reader :: Reader.MonadReader r m => (r -> a) -> ServerT m a
  reader = Morph.lift . Reader.reader

instance IO.MonadIO m => IO.MonadIO (ServerT m) where
  liftIO :: State.MonadIO m => IO a -> ServerT m a
  liftIO = Morph.lift . IO.liftIO

instance Morph.MonadTrans ServerT where
  lift :: Monad m => m a -> ServerT m a
  lift action = ServerT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor ServerT where
  hoist :: Monad m => (forall a. m a -> n a) -> ServerT m b -> ServerT n b
  hoist nat okapiT = ServerT . Except.ExceptT . State.StateT $ nat . State.runStateT (Except.runExceptT $ runServerT okapiT)

instance Monad m => Request.StateM (ServerT m) where
  get :: Monad m => ServerT m Request.Request
  get =
    ServerT
      . Except.ExceptT
      . State.StateT
      $ \state@(req, _) -> pure (Right req, state)
  put :: Monad m => Request.Request -> ServerT m ()
  put newReq =
    ServerT
      . Except.ExceptT
      . State.StateT
      $ \(_, res) -> pure (Right (), (newReq, res))

instance Monad m => Response.StateM (ServerT m) where
  get :: Monad m => ServerT m Response.Response
  get =
    ServerT
      . Except.ExceptT
      . State.StateT
      $ \state@(_, res) -> pure (Right res, state)
  put :: Monad m => Response.Response -> ServerT m ()
  put newRes =
    ServerT
      . Except.ExceptT
      . State.StateT
      $ \(req, _) -> pure (Right (), (req, newRes))

class (Request.RequestM m, Response.ResponseM m) => ServerM m

static :: ServerM m => m () -- TODO: Check file extension to set correct content type
static = do
  filePathText <- Text.intercalate "/" <$> Request.path
  let filePath = Text.unpack filePathText
  Response.setBodyFile filePath

-- | Parses without modifying the state, even if it succeeds.
look :: (Response.StateM m, Request.StateM m) => m a -> m a
look = Response.look . Request.look
