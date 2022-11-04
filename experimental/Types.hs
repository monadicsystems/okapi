{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Okapi.Types where

import qualified Control.Applicative as Applicative
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Strict as State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP

{-
TODO: HTTPDataStore? Not really needed because you can just pass data normally or store in own monad.
One benefit is that the data is available to all sub-branches without explicitly passing them to every sub-branch.

-- This data structure should be hidden from user
data HTTPDataStore = HTTPDataStore
  { pathStore   :: (Map Text Text)
  , queryStore  :: (Map Text Text)
  , headerStore :: (Map Text Text)
  }

-- Can only store parsed information
storePathParam :: forall a. FromHttpApiData a => Text -> Okapi a
storePathParam = ...

storeQueryParam :: ... => Text -> Okapi a

storeHeader :: ... => Text -> Okapi a

-- Can fail on Map lookup and data conversion
findPathParam :: forall a. FromHttpApiData a => Okapi a
-}

type MonadHTTP m =
  ( Functor m,
    Applicative m,
    Applicative.Alternative m,
    Monad m,
    Monad.MonadPlus m,
    Except.MonadError Failure m,
    State.MonadState State m
  )

newtype ServerT m a = ServerT {runServerT :: Except.ExceptT Failure (State.StateT State m) a}
  deriving newtype
    ( Except.MonadError Failure,
      State.MonadState State
    )

instance Functor m => Functor (ServerT m) where
  fmap :: (a -> b) -> ServerT m a -> ServerT m b
  fmap f okapiT =
    ServerT . Except.ExceptT . State.StateT $
      ( fmap (\ ~(a, s') -> (f <$> a, s'))
          . State.runStateT (Except.runExceptT $ runServerT okapiT)
      )
  {-# INLINE fmap #-}

instance Monad m => Applicative (ServerT m) where
  pure x = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
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
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance Monad m => Applicative.Alternative (ServerT m) where
  empty = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Left Skip, s)
  {-# INLINE empty #-}
  (ServerT (Except.ExceptT (State.StateT mx))) <|> (ServerT (Except.ExceptT (State.StateT my))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Skip -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Skip -> pure (Left Skip, s)
          Left error@(Error _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Error _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE (<|>) #-}

instance Monad m => Monad (ServerT m) where
  return = pure
  {-# INLINEABLE return #-}
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
  mzero = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Left Skip, s)
  {-# INLINE mzero #-}
  (ServerT (Except.ExceptT (State.StateT mx))) `mplus` (ServerT (Except.ExceptT (State.StateT my))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Skip -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Skip -> pure (Left Skip, s)
          Left error@(Error _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Error _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance Reader.MonadReader r m => Reader.MonadReader r (ServerT m) where
  ask = Morph.lift Reader.ask
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either Failure a, State) -> n (Either Failure b, State)) -> ServerT m a -> ServerT n b
      mapOkapiT f okapiT = ServerT . Except.ExceptT . State.StateT $ f . State.runStateT (Except.runExceptT $ runServerT okapiT)
  reader = Morph.lift . Reader.reader

instance Morph.MonadTrans ServerT where
  lift :: Monad m => m a -> ServerT m a
  lift action = ServerT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor ServerT where
  hoist :: Monad m => (forall a. m a -> n a) -> ServerT m b -> ServerT n b
  hoist nat okapiT = ServerT . Except.ExceptT . State.StateT $ (nat . State.runStateT (Except.runExceptT $ runServerT okapiT))

data State = State
  { stateRequest :: Request,
    -- TODO: Remove state checkers??
    stateRequestMethodParsed :: Bool, -- Use Maybe instead of State Checks
    stateRequestBodyParsed :: Bool,
    stateResponded :: Bool,
    stateVault :: Vault.Vault
    -- add HTTPDataStore???
  }

data Request = Request
  { requestMethod :: Method,
    requestPath :: Path,
    requestQuery :: Query,
    requestBody :: Body,
    requestHeaders :: Headers
  }

data Response = Response
  { responseStatus :: Status,
    responseHeaders :: Headers,
    responseBody :: ResponseBody
  }

data ResponseBody
  = ResponseBodyRaw LBS.ByteString
  | ResponseBodyFile FilePath
  | ResponseBodyEventSource EventSource

-- TODO: ADD Text field to Skip for logging
data Failure = Skip | Error Response

instance Show Failure where
  show Skip = "Skipped"
  show (Error _) = "Error returned"

type Method = HTTP.Method

type Path = [Text.Text]

type Query = [QueryItem]

type QueryItem = (Text, QueryValue)

data QueryValue = QueryParam Text | QueryFlag deriving (Eq, Show) -- QueryList [Text]

type Body = LBS.ByteString

type Headers = [Header]

type Header = (HeaderName, BS.ByteString)

type HeaderName = HTTP.HeaderName

type Cookie = (Text.Text, Text.Text)

type Cookies = [Cookie]

type Status = Natural.Natural

class ToSSE a where
  toSSE :: a -> Event

data Event
  = Event
      { eventName :: Maybe Text.Text,
        eventID :: Maybe Text.Text,
        eventData :: LBS.ByteString
      }
  | CommentEvent LBS.ByteString
  | CloseEvent
  deriving (Show, Eq)

type Chan a = (Unagi.InChan a, Unagi.OutChan a)

type EventSource = Chan Event

newtype URL = URL {unURL :: Text}
  deriving newtype (IsString, Semigroup, Monoid, Eq, Ord, Show)
