{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Okapi.Error as Error
import qualified Okapi.Request as Request
import qualified Okapi.Request.Body as Body
import qualified Okapi.Request.Headers as Headers
import qualified Okapi.Request.Method as Method
import qualified Okapi.Request.Path as Path
import qualified Okapi.Request.Query as Query
import qualified Okapi.Request.Vault as Vault
import qualified Okapi.Response as Response

type MonadHTTP m = (Except.MonadError Error.Error m, Logger.MonadLogger m, Monad.MonadPlus m, State m)

class (Request.State m, Response.State m) => State m where
  get :: m (Request.Request, Response.Response)
  default get :: m (Request.Request, Response.Response)
  get = (,) <$> Request.get <*> Response.get
  put :: (Request.Request, Response.Response) -> m ()
  default put :: (Request.Request, Response.Response) -> m ()
  put (request, response) = Request.put request >> Response.put response

static :: (Path.MonadPath m, Response.MonadResponse m) => m () -- TODO: Check file extension to set correct content type
static = do
  filePathText <- Text.intercalate "/" <$> Path.path
  let filePath = Text.unpack filePathText
  Response.setBodyFile filePath

look :: State m => m a -> m a
look action = do
  requestAndResponse <- get
  result <- action
  put requestAndResponse
  pure result

-- | A concrete implementation of the @MonadHTTP@ type constraint.
newtype HTTPT m a = HTTPT {runServerT :: Except.ExceptT Error.Error (State.StateT (Request.Request, Response.Response) m) a}
  deriving newtype
    ( Except.MonadError Error.Error
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
  empty = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Left Error.Next, s)
  {-# INLINE empty #-}
  (<|>) :: Monad m => HTTPT m a -> HTTPT m a -> HTTPT m a
  (HTTPT (Except.ExceptT (State.StateT mx))) <|> (HTTPT (Except.ExceptT (State.StateT my))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Error.Next -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Error.Next -> pure (Left Error.Next, s)
          Left error@Error.Error {} -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@Error.Error {} -> pure (Left error, s)
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
  mzero = HTTPT . Except.ExceptT . State.StateT $ \s -> pure (Left Error.Next, s)
  {-# INLINE mzero #-}
  mplus :: Monad m => HTTPT m a -> HTTPT m a -> HTTPT m a
  (HTTPT (Except.ExceptT (State.StateT mx))) `mplus` (HTTPT (Except.ExceptT (State.StateT my))) = HTTPT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Error.Next -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Error.Next -> pure (Left Error.Next, s)
          Left error@Error.Error {} -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@Error.Error {} -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance Reader.MonadReader r m => Reader.MonadReader r (HTTPT m) where
  ask :: Reader.MonadReader r m => HTTPT m r
  ask = Morph.lift Reader.ask
  local :: Reader.MonadReader r m => (r -> r) -> HTTPT m a -> HTTPT m a
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either Error.Error a, (Request.Request, Response.Response)) -> n (Either Error.Error b, (Request.Request, Response.Response))) -> HTTPT m a -> HTTPT n b
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

instance Monad m => Method.State (HTTPT m) where
  get :: Monad m => HTTPT m Method.Method
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.method req, s)
  put :: Monad m => Method.Method -> HTTPT m ()
  put method' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.method = method'}, res))

instance Monad m => Path.State (HTTPT m) where
  get :: Monad m => HTTPT m Path.Path
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.path req, s)
  put :: Monad m => Path.Path -> HTTPT m ()
  put path' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.path = path'}, res))

instance Monad m => Headers.State (HTTPT m) where
  get :: Monad m => HTTPT m Headers.Headers
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.headers req, s)
  put :: Monad m => Headers.Headers -> HTTPT m ()
  put headers' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.headers = headers'}, res))

instance Monad m => Query.State (HTTPT m) where
  get :: Monad m => HTTPT m Query.Query
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.query req, s)
  put :: Monad m => Query.Query -> HTTPT m ()
  put query' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.query = query'}, res))

instance Monad m => Body.State (HTTPT m) where
  get :: Monad m => HTTPT m Body.Body
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.body req, s)
  put :: Monad m => Body.Body -> HTTPT m ()
  put body' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.body = body'}, res))

instance Monad m => Vault.State (HTTPT m) where
  get :: Monad m => HTTPT m Vault.Vault
  get = HTTPT . Except.ExceptT . State.StateT $ \s@(req, res) -> pure (Right $ Request.vault req, s)
  put :: Monad m => Vault.Vault -> HTTPT m ()
  put vault' = HTTPT . Except.ExceptT . State.StateT $ \(req, res) -> pure (Right (), (req {Request.vault = vault'}, res))

instance Monad m => Response.State (HTTPT m) where
  get :: Monad m => HTTPT m Response.Response
  get =
    HTTPT
      . Except.ExceptT
      . State.StateT
      $ \state@(_, res) -> pure (Right res, state)
  put :: Monad m => Response.Response -> HTTPT m ()
  put newRes =
    HTTPT
      . Except.ExceptT
      . State.StateT
      $ \(req, _) -> pure (Right (), (req, newRes))
