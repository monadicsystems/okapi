{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Okapi.Mode where

type Contract :: (Type -> Type) -> Type -> Type
type family Contract part a = r | r -> part a where
    Contract Status a = StatusContract a

type Success :: (Type -> Type) -> Type -> Type
type family Success part a = r | r -> part a where
    Success Status a = a
