{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Spec.Request.Security where

import Control.Applicative (Alternative ((<|>)), empty)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi qualified as OAPI
import Data.Set.NonEmpty (NESet)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as Generics
import Network.HTTP.Types qualified as HTTP
import Okapi.Spec
import Web.Cookie qualified as Web
import Web.HttpApiData qualified as Web
import Okapi.Spec.Request.Security.Scheme qualified as Scheme

data Spec a where
  None :: Spec ()
  Lenient :: NonEmpty (Scheme.Spec a) -> Spec (Maybe a)
  Strict :: NonEmpty (Scheme.Spec a) -> Spec a

data Error
  = SchemeError Scheme.Error
  deriving (Eq, Show, Generics.Generic)

eval ::
  Spec a ->
  Scheme.State ->
  (Result Error a, Scheme.State)
eval spec state = case spec of
  None -> (Ok (), state)
  Lenient (scheme :| tail) -> case Scheme.eval scheme state of
    (Ok value, state') -> (Ok $ Just value, state')
    (Fail error, state') -> case tail of
      [] -> (Ok Nothing, state')
      (h:t) -> eval (lenient $ h :| t) state 
  Strict (scheme :| tail) -> case Scheme.eval scheme state of
    (Ok value, state') -> (Ok value, state')
    (Fail error, state') -> case tail of
      [] -> (Fail $ SchemeError error, state')
      (h:t) -> eval (strict $ h :| t) state

lenient = Lenient

strict = Strict

class Interface a where
  parser :: Spec a

-- countOps :: Spec a -> Int
-- countOps path = case path of
--   FMap _ opX -> countOps opX
--   Pure _ -> 0
--   Apply opF opX -> countOps opF + countOps opX
--   Static _ -> 1
--   Param _ -> 1
