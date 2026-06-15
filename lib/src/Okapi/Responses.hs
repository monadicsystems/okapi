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

module Okapi.Responses where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, Rep, S1, (:+:) (..))
import Okapi.Codec (Codec (..), IsoCodec (..), Value)
import Okapi.Response (Response)


-- Internal codec tree for response alternatives

data Responses a where
    Only   :: Response IsoCodec s h b -> Responses (Response Value s h b)
    Choice :: Responses a -> Responses b -> Responses (Either a b)


-- GResponseAlt: walks the Generic Rep, building the Responses codec tree

type family GResponseFunc (f :: Type -> Type) (r :: Type) :: Type where
    GResponseFunc (D1 m f)                             r = GResponseFunc f r
    GResponseFunc (C1 m f)                             r = GResponseFunc f r
    GResponseFunc (S1 m (Rec0 (Response Value s h b))) r = Response IsoCodec s h b -> r
    GResponseFunc (f :+: g)                            r = GResponseFunc f (GResponseFunc g r)

class GResponseAlt (f :: Type -> Type) where
    type GResponseOut f :: Type
    gResponseCase :: forall r. Proxy f -> (Responses (GResponseOut f) -> r) -> GResponseFunc f r
    gResponseTo   :: Proxy f -> GResponseOut f -> f ()
    gResponseFrom :: f () -> GResponseOut f

runGResponseCase :: forall f r. GResponseAlt f => (Responses (GResponseOut f) -> r) -> GResponseFunc f r
runGResponseCase = gResponseCase (Proxy @f)

runGResponseTo :: forall f. GResponseAlt f => GResponseOut f -> f ()
runGResponseTo = gResponseTo (Proxy @f)

runGResponseFrom :: forall f. GResponseAlt f => f () -> GResponseOut f
runGResponseFrom = gResponseFrom

instance GResponseAlt f => GResponseAlt (D1 m f) where
    type GResponseOut (D1 m f) = GResponseOut f
    gResponseCase _ k   = runGResponseCase @f k
    gResponseTo _ x     = M1 (runGResponseTo @f x)
    gResponseFrom (M1 x) = runGResponseFrom @f x

instance GResponseAlt f => GResponseAlt (C1 m f) where
    type GResponseOut (C1 m f) = GResponseOut f
    gResponseCase _ k    = runGResponseCase @f k
    gResponseTo _ x      = M1 (runGResponseTo @f x)
    gResponseFrom (M1 x) = runGResponseFrom @f x

instance GResponseAlt (S1 m (Rec0 (Response Value s h b))) where
    type GResponseOut (S1 m (Rec0 (Response Value s h b))) = Response Value s h b
    gResponseCase _ k codec = k (Only codec)
    gResponseTo _ x         = M1 (K1 x)
    gResponseFrom (M1 (K1 x)) = x

instance (GResponseAlt f, GResponseAlt g) => GResponseAlt (f :+: g) where
    type GResponseOut (f :+: g) = Either (GResponseOut f) (GResponseOut g)
    gResponseCase _ k =
        runGResponseCase @f $ \raF ->
        runGResponseCase @g $ \raG ->
        k (Choice raF raG)
    gResponseTo _ (Left  x) = L1 (runGResponseTo @f x)
    gResponseTo _ (Right x) = R1 (runGResponseTo @g x)
    gResponseFrom (L1 x) = Left  (runGResponseFrom @f x)
    gResponseFrom (R1 x) = Right (runGResponseFrom @g x)


-- | Class for HKD sum response types; derive with @Generic@ to get 'responsesOf' for free.
class
    ( Generic (r Value)
    , GResponseAlt (Rep (r Value))
    ) =>
    ResponseEnum (r :: ((Type -> Type) -> Type -> Type) -> Type)
    where
    -- | Build the response codec by supplying one 'Response' codec per constructor, in order.
    responsesOf :: GResponseFunc (Rep (r Value)) (IsoCodec Responses (r Value))
    responsesOf =
        runGResponseCase @(Rep (r Value)) $ \ra ->
        IsoCodec $
        FMap ((to :: Rep (r Value) () -> r Value) . runGResponseTo @(Rep (r Value))) $
        LMap (runGResponseFrom @(Rep (r Value)) . (from :: r Value -> Rep (r Value) ())) $
        Lift ra
