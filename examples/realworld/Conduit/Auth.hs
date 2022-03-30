{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Auth where

import Conduit.Type (EncodedUser (..), MonadHandler, User (..), grab)
import Data.Aeson
import Data.Char (isSpace)
import Data.Int
import Data.Map as Map (fromList, (!?))
import Data.Text
import qualified Data.Text as Text
import Web.JWT as JWT

userIdToToken :: MonadHandler m => Int32 -> m Text
userIdToToken userID = do
  jwtSecret <- grab @Text
  return $ encodeUserID jwtSecret userID

encodeUser :: Text -> User -> EncodedUser
encodeUser secret User {..} =
  EncodedUser
    { encodedUserEmail = userEmail,
      encodedUserToken = encodeUserID secret userID,
      encodedUserUsername = userUsername,
      encodedUserBio = userBio,
      encodedUserImage = userImage,
      encodedUserCreatedAt = userCreatedAt,
      encodedUserUpdatedAt = userUpdatedAt
    }

encodeUserID :: Text -> Int32 -> Text
encodeUserID jwtSecret userID =
  encodeSigned
    (JWT.hmacSecret jwtSecret)
    mempty
    (mempty {unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, toJSON userID)]})

tokenToUserID :: MonadHandler m => Text -> m (Maybe Int32)
tokenToUserID token = do
  jwtSecret <- grab @Text
  pure $ verifyToken jwtSecret token

verifyToken :: Text -> Text -> Maybe Int32
verifyToken jwtSecret token = do
  jwt <- JWT.decodeAndVerifySignature (JWT.toVerify $ JWT.hmacSecret jwtSecret) token
  result <- fromJSON <$> unClaimsMap (JWT.unregisteredClaims (JWT.claims jwt)) !? jwtKey
  case result of
    (Success userID) -> Just userID
    _ -> Nothing

jwtKey :: Text
jwtKey = "jwt"

extractToken :: Text -> Maybe Text
extractToken auth
  | toLower x == "token" = Just $ Text.dropWhile isSpace y
  | otherwise = Nothing
  where
    (x, y) = Text.break isSpace auth
