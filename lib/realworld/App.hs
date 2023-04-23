{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Lens
import Crypto.JWT
import qualified Crypto.JWT as JWT
import Data (Username)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as M
import qualified Data.Text as Text
import Data.Time (getCurrentTime)

data Claims = Claims {claims :: ClaimsSet, username :: Username}

instance JWT.HasClaimsSet Claims where
  claimsSet f s = fmap (\a' -> s {claims = a'}) (f (claims s))

instance Aeson.FromJSON Claims where
  parseJSON = Aeson.withObject "Claims" $ \o ->
    Claims
      <$> Aeson.parseJSON (Aeson.Object o)
      <*> o
      Aeson..: "username"

instance Aeson.ToJSON Claims where
  toJSON s =
    ins "username" (username s) (Aeson.toJSON (claims s))
    where
      ins k v (Aeson.Object o) = Aeson.Object $ M.insert k (Aeson.toJSON v) o
      ins _ _ a = a

mkClaims :: IO ClaimsSet
mkClaims = do
  t <- getCurrentTime
  pure $
    emptyClaimsSet
      & claimIss ?~ "realworld"
      & claimAud ?~ Audience ["realworld"]
      & claimIat ?~ NumericDate t

doJwtSign :: JWK -> Claims -> IO (Either JWTError SignedJWT)
doJwtSign jwk claims = runJOSE $ do
  alg <- bestJWSAlg jwk
  JWT.signJWT jwk (newJWSHeader ((), alg)) claims

doJwtVerify :: JWK -> SignedJWT -> IO (Either JWTError Claims)
doJwtVerify jwk jwt = runJOSE $ do
  let config = defaultJWTValidationSettings (== "realworld")
  JWT.verifyJWT config jwk jwt
