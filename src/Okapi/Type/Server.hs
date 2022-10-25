{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Type.Server where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Strict as State
import qualified Okapi.State.Request as Request
import qualified Okapi.State.Response as Response
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request
import qualified Okapi.Type.Response as Response

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

instance Logger.MonadLogger m => Logger.MonadLogger (ServerT m) where
  monadLoggerLog :: Logger.ToLogStr msg => Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> ServerT m ()
  monadLoggerLog loc logSrc logLvl msg = Morph.lift $ Logger.monadLoggerLog loc logSrc logLvl msg

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
