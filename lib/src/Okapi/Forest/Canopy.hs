{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Okapi.Forest.Canopy (
    Signature,
    Canopy (..),
) where

import Data.Kind (Type)
import Okapi.Mode.Tree qualified as Tree
import Okapi.HTTP.Responses (Cases, Only, Responses)

data Signature
    (method    :: Type)
    (path      :: Type)
    (query     :: Type)
    (headers   :: Type)
    (body      :: Type)
    (responses :: (Type -> Type -> Type -> Type) -> Type)

data Canopy sig where
    (:->) ::
        Tree.Request method path query headers body ->
        Tree.Response status resHeaders resBody ->
        Canopy (Signature method path query headers body (Only status resHeaders resBody))
    (:-<) ::
        Cases responses =>
        Tree.Request method path query headers body ->
        Responses Tree.Response responses ->
        Canopy (Signature method path query headers body responses)
