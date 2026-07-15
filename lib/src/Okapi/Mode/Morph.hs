module Okapi.Mode.Morph (Morph (..), morph) where

import Data.Kind (Type)

{- | Lifts a shape-only, monad-agnostic type ('Okapi.Mode.Contract.Contract',
  'Okapi.Mode.Link.Link', 'Okapi.Mode.Client.Client') into the 2-arg slot a
  heterogeneous HKD record needs — @n@ is carried but never inspected,
  purely so each field of such a record can bake in its own @n@ the same
  way it already bakes in its own @shape@.
-}
newtype Morph (f :: Type -> Type) (n :: Type -> Type) shape = Morph (f shape)

morph :: f shape -> Morph f n shape
morph = Morph
