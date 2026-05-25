{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.ResAlt where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, Rep, S1, (:+:) (..))
import Okapi.Codec (Codec (..), IsoCodec (..), Value)
import Okapi.Res (Res)

-- ---------------------------------------------------------------------------
-- ResAlt GADT

data ResAlt a where
    OneResAlt    :: Res IsoCodec s h b -> ResAlt (Res Value s h b)
    ChoiceResAlt :: ResAlt a -> ResAlt b -> ResAlt (Either a b)

-- ---------------------------------------------------------------------------
-- only: single-response smart constructor
--
-- Lets endpoints with a single response skip defining a sum type:
--
--   myReq :-> only myRes

newtype Only s h b (f :: (Type -> Type) -> Type -> Type) = Only {runOnly :: Res f s h b}

only :: Res IsoCodec s h b -> IsoCodec ResAlt (Only s h b Value)
only res = IsoCodec $ FMap Only $ LMap runOnly $ Embed (OneResAlt res)

-- ---------------------------------------------------------------------------
-- GResFunc: curried codec-constructor type for each generic rep node

type family GResFunc (f :: Type -> Type) (r :: Type) :: Type where
    GResFunc (D1 m f)                        r = GResFunc f r
    GResFunc (C1 m f)                        r = GResFunc f r
    GResFunc (S1 m (Rec0 (Res Value s h b))) r = Res IsoCodec s h b -> r
    GResFunc (f :+: g)                       r = GResFunc f (GResFunc g r)

-- ---------------------------------------------------------------------------
-- GResAlt: walks the generic Rep, building the codec tree
--
-- GResOut and GResFunc are non-injective, so GHC cannot solve the instance
-- variable from the method's return type alone.  Each method takes an
-- explicit Proxy f to pin the instance variable by value-level evidence.

class GResAlt (f :: Type -> Type) where
    type GResOut f :: Type
    gResCase :: forall r. Proxy f -> (ResAlt (GResOut f) -> r) -> GResFunc f r
    gTo      :: Proxy f -> GResOut f -> f ()
    gFrom    :: f () -> GResOut f

-- Relay helpers: supply the Proxy via TypeApplications at call sites.
runGResCase :: forall f r. GResAlt f => (ResAlt (GResOut f) -> r) -> GResFunc f r
runGResCase = gResCase (Proxy @f)

runGTo :: forall f. GResAlt f => GResOut f -> f ()
runGTo = gTo (Proxy @f)

runGFrom :: forall f. GResAlt f => f () -> GResOut f
runGFrom = gFrom

-- D1 / C1 — transparent wrappers

instance GResAlt f => GResAlt (D1 m f) where
    type GResOut (D1 m f) = GResOut f
    gResCase _ k   = runGResCase @f k
    gTo _ x        = M1 (runGTo @f x)
    gFrom (M1 x)   = runGFrom @f x

instance GResAlt f => GResAlt (C1 m f) where
    type GResOut (C1 m f) = GResOut f
    gResCase _ k   = runGResCase @f k
    gTo _ x        = M1 (runGTo @f x)
    gFrom (M1 x)   = runGFrom @f x

-- S1 leaf — single Res field

instance GResAlt (S1 m (Rec0 (Res Value s h b))) where
    type GResOut (S1 m (Rec0 (Res Value s h b))) = Res Value s h b
    gResCase _ k codec = k (OneResAlt codec)
    gTo _ x            = M1 (K1 x)
    gFrom (M1 (K1 x))  = x

-- :+: — build ChoiceResAlt; nesting collects all codec arguments

instance (GResAlt f, GResAlt g) => GResAlt (f :+: g) where
    type GResOut (f :+: g) = Either (GResOut f) (GResOut g)
    gResCase _ k =
        runGResCase @f $ \raF ->
        runGResCase @g $ \raG ->
        k (ChoiceResAlt raF raG)
    gTo _ (Left  x) = L1 (runGTo @f x)
    gTo _ (Right x) = R1 (runGTo @g x)
    gFrom (L1 x)    = Left  (runGFrom @f x)
    gFrom (R1 x)    = Right (runGFrom @g x)

-- ---------------------------------------------------------------------------
-- GenericResAlt: user-facing class for sum response types

class
    ( Generic (r Value)
    , GResAlt (Rep (r Value))
    ) =>
    GenericResAlt (r :: ((Type -> Type) -> Type -> Type) -> Type)
    where
    resCase :: GResFunc (Rep (r Value)) (IsoCodec ResAlt (r Value))
    resCase =
        runGResCase @(Rep (r Value)) $ \ra ->
        IsoCodec $
        FMap ((to :: Rep (r Value) () -> r Value) . runGTo @(Rep (r Value))) $
        LMap (runGFrom @(Rep (r Value)) . (from :: r Value -> Rep (r Value) ())) $
        Embed ra
