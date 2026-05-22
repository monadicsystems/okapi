{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.ResAlt where

import Data.Kind (Type)
import GHC.Generics (C1, D1, Generic (..), Rec0, Rep, S1, U1, V1, (:*:), (:+:))
import Okapi.Codec (IsoCodec, Value)
import Okapi.Res (Res)

data ResAlt a where
    OneResAlt ::
        Res IsoCodec s h b ->
        ResAlt (Res Value s h b)
    NestedResAlt ::
        IsoCodec ResAlt (r Value) ->
        ResAlt (r Value)
    ChoiceResAlt ::
        ResAlt a ->
        ResAlt b ->
        ResAlt (Either a b)

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

type family GCode (f :: Type -> Type) :: [[Type]] where
    GCode (D1 _ f) = GCode f
    GCode (f :+: g) = GCode f ++ GCode g
    GCode (C1 _ f) = '[GFields f]
    GCode V1 = '[]

type family GFields (f :: Type -> Type) :: [Type] where
    GFields (f :*: g) = GFields f ++ GFields g
    GFields (S1 _ f) = '[GField f]
    GFields U1 = '[]

type family GField (f :: Type -> Type) :: Type where
    GField (Rec0 a) = a

class ResCodecC (xs :: [Type]) where
    type ResCodecT xs :: Type

instance {-# OVERLAPPING #-} ResCodecC '[Res Value s h b] where
    type ResCodecT '[Res Value s h b] = Res IsoCodec s h b

instance (GenericResAlt r) => ResCodecC '[r Value] where
    type ResCodecT '[r Value] = IsoCodec ResAlt (r Value)

type family ResCodecs (xss :: [[Type]]) :: [Type] where
    ResCodecs '[] = '[]
    ResCodecs (xs : xss) = ResCodecT xs : ResCodecs xss

type family ResChains (args :: [Type]) (result :: Type) :: Type where
    ResChains '[] result = result
    ResChains (a : as) result = a -> ResChains as result

class
    (Generic (r Value)) =>
    GenericResAlt (r :: ((Type -> Type) -> Type -> Type) -> Type)
    where
    resCase ::
        ResChains
            (ResCodecs (GCode (Rep (r Value))))
            (IsoCodec ResAlt (r Value))
    resCase = undefined
