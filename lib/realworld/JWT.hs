{-# LANGUAGE OverloadedStrings #-}

module JWT where

import Control.Lens
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JWT as JWT
import Data (Username)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as M
import qualified Data.Text as Text
import Data.Text.Strict.Lens (utf8)
import Data.Time (getCurrentTime)

data Claims = Claims {claims :: JWT.ClaimsSet, username :: Username}

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

doClaims :: IO JWT.ClaimsSet
doClaims = do
  t <- getCurrentTime
  pure $
    JWT.emptyClaimsSet
      & JWT.claimIss ?~ "realworld"
      & JWT.claimAud ?~ JWT.Audience ["realworld"]
      & JWT.claimIat ?~ JWT.NumericDate t

doSign :: JWT.JWK -> Claims -> IO (Either JWT.JWTError JWT.SignedJWT)
doSign jwk claims = JWT.runJOSE $ do
  alg <- JWT.bestJWSAlg jwk
  JWT.signJWT jwk (JWT.newJWSHeader ((), alg)) claims

doVerify :: JWT.JWK -> JWT.SignedJWT -> IO (Either JWT.JWTError Claims)
doVerify jwk jwt = JWT.runJOSE $ do
  let config = JWT.defaultJWTValidationSettings (== "realworld")
  JWT.verifyJWT config jwk jwt

-- Generate RSA JWK and set "kid" param to
-- base64url-encoded SHA-256 thumbprint of key.
--
doGenKey :: IO JWK.JWK
doGenKey = do
  jwk <- JWK.genJWK (JWK.RSAGenParam (4096 `div` 8))
  let h = view JWK.thumbprint jwk :: JWK.Digest JWK.SHA256
      kid = view (re (JWK.base64url . JWK.digest) . utf8) h
  pure $ set JWK.jwkKid (Just kid) jwk