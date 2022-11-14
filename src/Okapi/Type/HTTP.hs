{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Type.HTTP where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Strict as State
import qualified Okapi.State.Request.Body as Body
import qualified Okapi.State.Request.Headers as Headers
import qualified Okapi.State.Request.Method as Method
import qualified Okapi.State.Request.Path as Path
import qualified Okapi.State.Request.Query as Query
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request
import qualified Okapi.Type.Response as Response
import qualified Okapi.State.Request.Vault as Vault
import qualified Data.Vault.Lazy as Vault

-- | A concrete implementation of the @MonadHTTP@ type constraint.
newtype HTTPT m a = HTTPT {runServerT :: Except.ExceptT Failure.Failure (State.StateT (Request.Request, Response.Response) m) a}
  deriving newtype
    ( Except.MonadError Failure.Failure
    )

instance Functor m => Functor (HTTPT m) where
  fmap :: (a -> b) -> HTTPT m a -> HTTPT m b
  fmap f okapiT =
    HTTPT . Except.ExceptT . State.StateT $
      fmap (\ ~(a, s') -> (f <$> a, s'))
        . State.runStateT (Except.runExceptT $ runServerT okapiT)
  {-# INLINE fmap #-}

instance Monad m => Applicative (HTTPT m) where
  pure :: Monad m => a -> HTTPT m a
  pure x = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
  (<*>) :: Monad m => HTTPT m (a -> b) -> HTTPT m a -> HTTPT m b
  (HTTPT (Except.ExceptT (State.StateT mf))) <*> (HTTPT (Except.ExceptT (State.StateT mx))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherF, s') <- mf s
    case eitherF of
      Left error -> pure (Left error, s)
      Right f -> do
        ~(eitherX, s'') <- mx s'
        case eitherX of
          Left error' -> pure (Left error', s')
          Right x -> pure (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}
  (*>) :: Monad m => HTTPT m a -> HTTPT m b -> HTTPT m b
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance Monad m => Applicative.Alternative (HTTPT m) where
  empty :: Monad m => HTTPT m a
  empty = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Left Failure.Next, s)
  {-# INLINE empty #-}
  (<|>) :: Monad m => HTTPT m a -> HTTPT m a -> HTTPT m a
  (HTTPT (Except.ExceptT (State.StateT mx))) <|> (HTTPT (Except.ExceptT (State.StateT my))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
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

instance Monad m => Monad (HTTPT m) where
  return :: Monad m => a -> HTTPT m a
  return = pure
  {-# INLINEABLE return #-}
  (>>=) :: Monad m => HTTPT m a -> (a -> HTTPT m b) -> HTTPT m b
  (HTTPT (Except.ExceptT (State.StateT mx))) >>= f = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    ~(eitherX, s') <- mx s
    case eitherX of
      Left error -> pure (Left error, s)
      Right x -> do
        ~(eitherResult, s'') <- State.runStateT (Except.runExceptT $ runServerT $ f x) s'
        case eitherResult of
          Left error' -> pure (Left error', s')
          Right res -> pure (Right res, s'')
  {-# INLINEABLE (>>=) #-}

instance Monad m => Monad.MonadPlus (HTTPT m) where
  mzero :: Monad m => HTTPT m a
  mzero = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Left Failure.Next, s)
  {-# INLINE mzero #-}
  mplus :: Monad m => HTTPT m a -> HTTPT m a -> HTTPT m a
  (HTTPT (Except.ExceptT (State.StateT mx))) `mplus` (HTTPT (Except.ExceptT (State.StateT my))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
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

instance Reader.MonadReader r m => Reader.MonadReader r (HTTPT m) where
  ask :: Reader.MonadReader r m => HTTPT m r
  ask = Morph.lift Reader.ask
  local :: Reader.MonadReader r m => (r -> r) -> HTTPT m a -> HTTPT m a
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either Failure.Failure a, (Request.Request, Response.Response)) -> n (Either Failure.Failure b, (Request.Request, Response.Response))) -> HTTPT m a -> HTTPT n b
      mapOkapiT f okapiT = HTTPT . Except.ExceptT . State.StateT $ f . State.runStateT (Except.runExceptT $ runServerT okapiT)
  reader :: Reader.MonadReader r m => (r -> a) -> HTTPT m a
  reader = Morph.lift . Reader.reader

instance Logger.MonadLogger m => Logger.MonadLogger (HTTPT m) where
  monadLoggerLog :: Logger.ToLogStr msg => Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> HTTPT m ()
  monadLoggerLog loc logSrc logLvl msg = Morph.lift $ Logger.monadLoggerLog loc logSrc logLvl msg

instance IO.MonadIO m => IO.MonadIO (HTTPT m) where
  liftIO :: IO.MonadIO m => IO a -> HTTPT m a
  liftIO = Morph.lift . IO.liftIO

instance Morph.MonadTrans HTTPT where
  lift :: Monad m => m a -> HTTPT m a
  lift action = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor HTTPT where
  hoist :: Monad m => (forall a. m a -> n a) -> HTTPT m b -> HTTPT n b
  hoist nat okapiT = HTTPT . Except.ExceptT . State.StateT $ nat . State.runStateT (Except.runExceptT $ runServerT okapiT)

instance Monad m => Method.MonadState (HTTPT m) where
  get :: Monad m => HTTPT m Request.Method
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.method req, s)
  put :: Monad m => Request.Method -> HTTPT m ()
  put method' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.method = method'}, res))

instance Monad m => Path.MonadState (HTTPT m) where
  get :: Monad m => HTTPT m Request.Path
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.path req, s)
  put :: Monad m => Request.Path -> HTTPT m ()
  put path' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.path = path'}, res))

instance Monad m => Headers.MonadState (HTTPT m) where
  get :: Monad m => HTTPT m Request.Headers
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.headers req, s)
  put :: Monad m => Request.Headers -> HTTPT m ()
  put headers' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.headers = headers'}, res))

instance Monad m => Query.MonadState (HTTPT m) where
  get :: Monad m => HTTPT m Request.Query
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.query req, s)
  put :: Monad m => Request.Query -> HTTPT m ()
  put query' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.query = query'}, res))

instance Monad m => Body.MonadState (HTTPT m) where
  get :: Monad m => HTTPT m Request.Body
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.body req, s)
  put :: Monad m => Request.Body -> HTTPT m ()
  put body' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.body = body'}, res))

instance Monad m => Vault.MonadState (HTTPT m) where
  get :: Monad m => HTTPT m Vault.Vault
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.vault req, s)
  put :: Monad m => Vault.Vault -> HTTPT m ()
  put vault' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.vault = vault'}, res))
