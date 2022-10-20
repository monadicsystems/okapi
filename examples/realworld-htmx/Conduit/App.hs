module Conduit.App where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Database.Redis as Redis
import qualified GHC.Generics as Generics
-- import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool as Hasql
import qualified Okapi
import qualified System.Random as Random
import qualified Conduit.Model.User as Model

newtype App a = App
  { unApp :: Reader.ReaderT AppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, IO.MonadIO, Reader.MonadReader AppEnv)

execute :: AppEnv -> App a -> IO a
execute appEnv (App app) = Reader.runReaderT app appEnv

data AppEnv = AppEnv
  { appEnvDBPool :: Hasql.Pool,
    appEnvRedisConnection :: Redis.Connection
  }

class Has field env where
  obtain :: env -> field

instance Has Hasql.Pool AppEnv where
  obtain = appEnvDBPool

instance Has Redis.Connection AppEnv where
  obtain = appEnvRedisConnection

grab :: forall field env m. (Reader.MonadReader env m, Has field env) => m field
grab = Reader.asks $ obtain @field

-- data Session = Session
--   { sessionUsername :: Text.Text,
--     sessionEmail :: Text.Text
--   }
--   deriving (Eq, Show, Generics.Generic, Aeson.ToJSON, Aeson.FromJSON)

instance Okapi.MonadSession App Model.User where
  sessionSecret :: App BS.ByteString
  sessionSecret = IO.liftIO $ BS.readFile "secret.txt"
  generateSessionID :: App Okapi.SessionID
  generateSessionID = IO.liftIO $ do
    words :: [Word.Word8] <- Monad.replicateM 20 Random.randomIO
    pure $ Okapi.SessionID $ BS.pack words
  getSession :: Okapi.SessionID -> App (Maybe Model.User)
  getSession (Okapi.SessionID sessionID) = do
    redisConnection <- grab @Redis.Connection
    redisResult <- IO.liftIO $ Redis.runRedis redisConnection do
      eitherMbValueBS <- Redis.get sessionID
      pure $ case eitherMbValueBS of
        Left _ -> Nothing
        Right mbValueBS -> Aeson.decodeStrict =<< mbValueBS
    IO.liftIO $ Redis.disconnect redisConnection
    pure redisResult
  putSession :: Okapi.SessionID -> Model.User -> App ()
  putSession (Okapi.SessionID sessionID) user = do
    redisConnection <- grab @Redis.Connection
    _ <-
      IO.liftIO $
        Redis.runRedis redisConnection $
          Redis.set sessionID (LBS.toStrict $ Aeson.encode user)
    IO.liftIO $ Redis.disconnect redisConnection
  clearSession :: Okapi.SessionID -> App ()
  clearSession (Okapi.SessionID sessionID) = do
    redisConnection <- grab @Redis.Connection
    _ <- IO.liftIO $ Redis.runRedis redisConnection $ Redis.del [sessionID]
    IO.liftIO $ Redis.disconnect redisConnection
