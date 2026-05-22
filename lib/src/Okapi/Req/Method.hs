{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Okapi.Req.Method where

import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Kind qualified as Kind (METHOD (..))

data KnownMethod (m :: Kind.METHOD) where
    GET :: KnownMethod Kind.GET
    POST :: KnownMethod Kind.POST
    PUT :: KnownMethod Kind.PUT
    DELETE :: KnownMethod Kind.DELETE

type Method :: Type -> Type
data Method a where
    StdMethod :: Method HTTP.StdMethod
    Method :: forall (m :: Kind.METHOD). Method (KnownMethod m)
    -- Lit   :: Text -> Path ()
    -- Param :: (Typeable a, IsoHttpApiData a) => Path a
    -- Blob  :: (Typeable a, IsoHttpApiData a) => Path (NonEmpty a)
