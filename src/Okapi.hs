{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Lazy as StateT
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Internal as ILBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

-- | A concrete implementation of the @Server@ type constraint.
newtype ServerT m a = ServerT {runServerT :: Except.ExceptT (Maybe Wai.Response) (State.StateT Wai.Request m) a}
  deriving newtype
    ( Except.MonadError (Maybe Wai.Response),
      State.MonadState Wai.Request
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
  pure x = ServerT . Except.ExceptT . State.StateT $ \s -> return (Right x, s)
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
  empty = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Left Nothing, s)
  {-# INLINE empty #-}
  (<|>) :: Monad m => ServerT m a -> ServerT m a -> ServerT m a
  (ServerT (Except.ExceptT (State.StateT mx))) <|> (ServerT (Except.ExceptT (State.StateT my))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Nothing -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Nothing -> pure (Left Nothing, s)
          Left error@(Just _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Just _) -> pure (Left error, s)
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
  mzero = ServerT . Except.ExceptT . State.StateT $ \s -> pure (Left Nothing, s)
  {-# INLINE mzero #-}
  mplus :: Monad m => ServerT m a -> ServerT m a -> ServerT m a
  (ServerT (Except.ExceptT (State.StateT mx))) `mplus` (ServerT (Except.ExceptT (State.StateT my))) = ServerT . Except.ExceptT . State.StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Nothing -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Nothing -> pure (Left Nothing, s)
          Left error@(Just _) -> pure (Left error, s)
          Right y -> pure (Right y, stateY)
      Left error@(Just _) -> pure (Left error, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance Reader.MonadReader r m => Reader.MonadReader r (ServerT m) where
  ask :: Reader.MonadReader r m => ServerT m r
  ask = Morph.lift Reader.ask
  local :: Reader.MonadReader r m => (r -> r) -> ServerT m a -> ServerT m a
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either (Maybe Wai.Response) a, Wai.Request) -> n (Either (Maybe Wai.Response) b, Wai.Request)) -> ServerT m a -> ServerT n b
      mapOkapiT f okapiT = ServerT . Except.ExceptT . State.StateT $ f . State.runStateT (Except.runExceptT $ runServerT okapiT)
  reader :: Reader.MonadReader r m => (r -> a) -> ServerT m a
  reader = Morph.lift . Reader.reader

instance IO.MonadIO m => IO.MonadIO (ServerT m) where
  liftIO :: IO.MonadIO m => IO a -> ServerT m a
  liftIO = Morph.lift . IO.liftIO

instance Morph.MonadTrans ServerT where
  lift :: Monad m => m a -> ServerT m a
  lift action = ServerT . Except.ExceptT . State.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor ServerT where
  hoist :: Monad m => (forall a. m a -> n a) -> ServerT m b -> ServerT n b
  hoist nat okapiT = ServerT . Except.ExceptT . State.StateT $ nat . State.runStateT (Except.runExceptT $ runServerT okapiT)

-- | A type constraint representing monads that have the ability to parse an HTTP request.
type Server m =
  ( Functor m,
    Applicative m,
    Applicative.Alternative m,
    Monad m,
    Monad.MonadPlus m,
    State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  )

next ::
  forall m a.
  Except.MonadError (Maybe Wai.Response) m =>
  m a
next = Except.throwError Nothing

-- Parsers
-- method ::
--   forall m.
--   State.MonadState Wai.Request m =>
--   HTTP.Method ->
--   m ()
-- method m = do
--   m' <- State.gets Wai.requestMethod
--   if m == m'
--     then State.modify (\request -> request {Wai.requestMethod = ""})
--     else next
pattern GET :: HTTP.Method
pattern GET = "GET"

pattern POST :: HTTP.Method
pattern POST = "POST"

pattern PUT :: HTTP.Method
pattern PUT = "PUT"

pattern DELETE :: HTTP.Method
pattern DELETE = "DELETE"

method ::
  forall m.
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  m HTTP.Method
method = do
  m <- State.gets Wai.requestMethod
  if m == mempty
    then next
    else do
      State.modify (\request -> request {Wai.requestMethod = mempty})
      pure m

-- | Parses and discards a single path segment matching the given @Text@ value
pathParam ::
  forall a m.
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m,
    Web.FromHttpApiData a
  ) =>
  m a
pathParam = do
  maybePathHead <- State.gets (safeHead . Wai.pathInfo)
  case maybePathHead of
    Nothing -> next
    Just pathHead -> do
      State.modify (\request -> request {Wai.pathInfo = Prelude.drop 1 $ Wai.pathInfo request})
      maybe next pure (Web.parseUrlPieceMaybe pathHead)
  where
    safeHead :: forall a. [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

-- | Parses the value of a query parameter with the given type and name
queryParam ::
  forall a m.
  ( Web.FromHttpApiData a,
    State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  BS.ByteString ->
  m a
queryParam name = do
  query <- State.gets Wai.queryString
  let maybeQueryItem = Foldable.find (\(name', _) -> name == name') query
  case maybeQueryItem of
    Nothing -> next
    Just foundQueryItem@(_, maybeValue) ->
      case maybeValue of
        Nothing -> next
        Just valueBS -> do
          let valueText :: Text.Text = undefined
          case Web.parseQueryParamMaybe valueText of
            Nothing -> next
            Just parsedValue -> do
              State.modify (\request -> request {Wai.queryString = List.delete foundQueryItem query})
              pure parsedValue

queryFlag ::
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  BS.ByteString ->
  m ()
queryFlag name = do
  query <- State.gets Wai.queryString
  let maybeQueryItem = Foldable.find (\(name', _) -> name == name') query
  case maybeQueryItem of
    Nothing -> next
    Just foundQueryItem ->
      State.modify (\request -> request {Wai.queryString = List.delete foundQueryItem query})

json ::
  forall a m.
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m,
    IO.MonadIO m,
    Aeson.FromJSON a
  ) =>
  m a
json = do
  maybeContentType <- State.gets Wai.getRequestBodyType
  case maybeContentType of
    Nothing -> do
      -- For now assume it's JSON. Add check later TODO
      readRequestBody <- State.gets Wai.strictRequestBody
      requestBody <- IO.liftIO readRequestBody
      maybe next pure (Aeson.decode requestBody)
    Just _ -> next

-- TODO: Use FromForm
form ::
  forall m.
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m,
    IO.MonadIO m
  ) =>
  m ([Wai.Param], [Wai.File ILBS.ByteString])
form = do
  readBody <- State.gets (Wai.parseRequestBody Wai.lbsBackEnd)
  IO.liftIO readBody

bytes ::
  forall m.
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m,
    IO.MonadIO m
  ) =>
  m ILBS.ByteString
bytes = do
  readBody <- State.gets Wai.strictRequestBody
  IO.liftIO readBody

-- TODO: Use (FromHTTPApiData a) for return type??
headerParam ::
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  HTTP.HeaderName ->
  m BS.ByteString
headerParam headerName = do
  maybeHeader <- State.gets (Foldable.find (\(headerName', _) -> headerName == headerName') . Wai.requestHeaders)
  case maybeHeader of
    Nothing -> next
    Just header@(_, headerValue) -> do
      State.modify (\request -> request {Wai.requestHeaders = List.delete header $ Wai.requestHeaders request})
      pure headerValue

cookie ::
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  m Web.Cookies
cookie = do
  cookieValue <- headerParam "Cookie"
  pure $ Web.parseCookies cookieValue

-- TODO: Use (FromHTTPApiData a) for return type???
cookieParam ::
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  BS.ByteString ->
  m BS.ByteString
cookieParam name = do
  c <- cookie
  case List.lookup name c of
    Nothing -> next
    Just value -> do
      let crumb = (name, value)
      -- TODO: Needs testing to see if state is restored properly
      State.modify
        ( \request ->
            request
              { Wai.requestHeaders =
                  ( "Cookie",
                    LBS.toStrict $
                      Builder.toLazyByteString $
                        Web.renderCookies $
                          List.delete crumb c
                  )
                    : Wai.requestHeaders request
              }
        )
      pure value

basicAuth ::
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  m (Text.Text, Text.Text)
basicAuth = do
  authValue <- headerParam "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case Text.decodeBase64 encodedCreds of
        Left _ -> next
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> next
    _ -> next

match ::
  forall a m.
  ( Eq a,
    State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  m a ->
  a ->
  m ()
match parser goal = do
  result <- parser
  Monad.unless (result == goal) next

satisfies ::
  forall a m.
  ( State.MonadState Wai.Request m,
    Except.MonadError (Maybe Wai.Response) m
  ) =>
  m a ->
  (a -> Bool) ->
  m ()
satisfies parser predicate = do
  result <- parser
  Monad.unless (predicate result) next

look ::
  forall a m.
  (State.MonadState Wai.Request m) =>
  m a ->
  m a
look parser = do
  checkpoint <- State.get
  result <- parser
  State.put checkpoint
  pure result
