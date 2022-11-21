{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Middleware.Session
  ( SessionID (..),
    MonadSession (..),
    withSession,
    session,
    sessionID,
    decodeSessionID,
    encodeSessionID,
  )
where

import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Morph as Morph
import qualified Crypto.Hash as Crypto
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray as Memory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import qualified Data.Either as Either
import qualified Okapi.Error as Error
import qualified Okapi.HTTP as HTTP
import qualified Okapi.Request as Request
import qualified Okapi.Request.Body as Body
import qualified Okapi.Request.Headers as Headers
import qualified Okapi.Request.Method as Method
import qualified Okapi.Request.Path as Path
import qualified Okapi.Request.Query as Query
import qualified Okapi.Request.Vault as Vault
import qualified Okapi.Response as Response

newtype SessionID = SessionID {unSessionID :: BS.ByteString}
  deriving (Eq, Show)

class Monad m => MonadSession m s | m -> s where
  -- | A secret used for encrypting and decrypting
  -- the session id. This function should return the same value each time it's called.
  sessionSecret :: m BS.ByteString

  -- | A function for generating a random session ID.
  -- This function should return a different value each time it's called.
  generateSessionID :: m SessionID

  getSession :: SessionID -> m (Maybe s)
  putSession :: SessionID -> s -> m ()
  clearSession :: SessionID -> m ()
  modifySession :: SessionID -> (s -> s) -> m ()
  default modifySession :: SessionID -> (s -> s) -> m ()
  modifySession sessionID modifier = do
    maybeSession <- getSession sessionID
    case maybeSession of
      Nothing -> pure ()
      Just session -> do
        let newSession = modifier session
        putSession sessionID newSession
  createSession :: s -> m ()
  default createSession :: s -> m ()
  createSession newSession = do
    newSessionID <- generateSessionID
    putSession newSessionID newSession

instance MonadSession m s => MonadSession (HTTP.HTTPT m) s where
  sessionSecret :: MonadSession m s => HTTP.HTTPT m BS.ByteString
  sessionSecret = Morph.lift sessionSecret
  generateSessionID :: MonadSession m s => HTTP.HTTPT m SessionID
  generateSessionID = Morph.lift generateSessionID
  getSession :: MonadSession m s => SessionID -> HTTP.HTTPT m (Maybe s)
  getSession = Morph.lift . getSession
  putSession :: MonadSession m s => SessionID -> s -> HTTP.HTTPT m ()
  putSession sessionID = Morph.lift . putSession sessionID
  clearSession :: MonadSession m s => SessionID -> HTTP.HTTPT m ()
  clearSession = Morph.lift . clearSession

sessionID :: (Request.MonadRequest m, MonadSession m s) => m SessionID
sessionID = do
  encodedSessionID <- Headers.crumb "session_id"
  secret <- sessionSecret
  maybe Error.next pure (decodeSessionID secret encodedSessionID)

session :: (Request.MonadRequest m, MonadSession m s) => m s
session = do
  sessionID' <- sessionID
  maybeSession <- getSession sessionID'
  maybe Error.next pure maybeSession

decodeSessionID :: BS.ByteString -> BS.ByteString -> Maybe SessionID
decodeSessionID secret encodedSessionID =
  let (b64, serial) = BS.splitAt 44 encodedSessionID
      mbDigest :: Maybe (Crypto.Digest Crypto.SHA256) = Crypto.digestFromByteString $ Either.fromRight BS.empty $ BS.decodeBase64 b64
   in case mbDigest of
        Nothing -> Nothing
        Just digest ->
          if HMAC.hmacGetDigest (HMAC.hmac secret serial :: HMAC.HMAC Crypto.SHA256) == digest
            then Just $ SessionID serial
            else Nothing

encodeSessionID :: BS.ByteString -> SessionID -> BS.ByteString
encodeSessionID secret (SessionID sessionID) =
  let serial = sessionID
      digest = HMAC.hmacGetDigest $ HMAC.hmac secret serial :: Crypto.Digest Crypto.SHA256
      b64 = BS.encodeBase64' $ Memory.convert digest
   in b64 <> serial

withSession :: (HTTP.MonadHTTP m, MonadSession m s) => m () -> m ()
withSession handler = do
  mbSessionID <- HTTP.look $ Combinators.optional sessionID
  secret <- sessionSecret
  case mbSessionID of
    Nothing -> do
      sessionID <- generateSessionID
      handler
      Response.addSetCookie ("session_id", encodeSessionID secret sessionID)
    Just sessionID -> do
      handler
      Response.addSetCookie ("session_id", encodeSessionID secret sessionID)
