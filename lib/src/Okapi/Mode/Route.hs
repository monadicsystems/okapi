{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Mode.Route
    ( Route (..)
    , dimapRoute
    , GRoute
    , routes
    ) where

import Data.Kind (Type)
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import Okapi.Mode.Forest (Forest (..), Shape)
import Okapi.Mode.Server (Server (..))

data Route (n :: Type -> Type) shape where
    (:=) :: Forest shape -> Server n shape -> Route n shape

infixl 0 :=

dimapRoute ::
    (Forest (Shape method path query headers body result) -> Forest (Shape method' path' query' headers' body' result')) ->
    (Server n (Shape method path query headers body result) -> Server n (Shape method' path' query' headers' body' result')) ->
    (Route n (Shape method path query headers body result) -> Route n (Shape method' path' query' headers' body' result'))
dimapRoute forestMap serverMap (forest := srv) = forestMap forest := serverMap srv

class GRoute (n :: Type -> Type) (epF :: Type -> Type) (svF :: Type -> Type) (rtF :: Type -> Type) where
    gRoute :: epF () -> svF () -> rtF ()

instance GRoute n epF svF rtF => GRoute n (D1 dm epF) (D1 dm' svF) (D1 dm'' rtF) where
    gRoute (M1 ep) (M1 sv) = M1 (gRoute @n @epF @svF @rtF ep sv)

instance GRoute n epF svF rtF => GRoute n (C1 cm epF) (C1 cm' svF) (C1 cm'' rtF) where
    gRoute (M1 ep) (M1 sv) = M1 (gRoute @n @epF @svF @rtF ep sv)

instance (GRoute n epL svL rtL, GRoute n epR svR rtR)
    => GRoute n (epL :*: epR) (svL :*: svR) (rtL :*: rtR) where
    gRoute (epL :*: epR) (svL :*: svR) =
        gRoute @n @epL @svL @rtL epL svL :*: gRoute @n @epR @svR @rtR epR svR

instance GRoute n
    (S1 sm  (Rec0 (Forest (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Server n (Shape method path query headers body result))))
    (S1 sm'' (Rec0 (Route n (Shape method path query headers body result)))) where
    gRoute (M1 (K1 ep)) (M1 (K1 sv)) = M1 (K1 (ep := sv))

routes ::
    forall server n.
    ( Generic (server Forest)
    , Generic (server (Server n))
    , Generic (server (Route n))
    , GRoute n (Rep (server Forest)) (Rep (server (Server n))) (Rep (server (Route n)))
    ) =>
    server Forest ->
    server (Server n) ->
    server (Route n)
routes endpoints handlers =
    to (gRoute @n @(Rep (server Forest)) @(Rep (server (Server n))) (from endpoints) (from handlers))
