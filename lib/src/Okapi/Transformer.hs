
-- | A natural transformation between two functors, and a record-field-
--   friendly wrapper around one — used by 'Okapi.Server.serversVia' to let
--   each field of a heterogeneous record of servers supply its own @n@,
--   the way 'Okapi.Contract.Signature' already lets each field vary its own
--   shape.
module Okapi.Transformer (type (~>), Transformer (..)) where

import Data.Kind (Type)

type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a

newtype Transformer n shape = Transformer (n ~> IO)
