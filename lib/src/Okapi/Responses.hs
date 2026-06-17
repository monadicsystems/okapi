{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Okapi.Codec (IsoCodec (..), ParseError, Value)
import Okapi.Response (Response)


-- | Internal codec tree for response alternatives, paired with the two
--   Either-tree shapes it reconstructs into. @aE@ is the @r ParseError@
--   Either-tree, @aV@ the @r Value@ Either-tree; in every 'Only' both are
--   forced by the same @s h b@, so it is one structure recorded at two modes.
data Responses (r :: ((Type -> Type) -> Type -> Type) -> Type) aE aV where
    Only   :: Response IsoCodec s h b
           -> Responses r (Response ParseError s h b) (Response Value s h b)
    Choice :: Responses r aEl aVl -> Responses r aEr aVr
           -> Responses r (Either aEl aEr) (Either aVl aVr)


-- | Maps a Generic Rep to its nested-coproduct ("generic case") view:
--   @:+:@ becomes 'Either', a leaf becomes its 'Response'. Mode-polymorphic
--   at the leaf, so the same shape describes @r Value@, @r ParseError@, …
type family GResponseOut (f :: Type -> Type) :: Type where
    GResponseOut (D1 m f)                         = GResponseOut f
    GResponseOut (C1 m f)                         = GResponseOut f
    GResponseOut (S1 m (Rec0 (Response g s h b))) = Response g s h b
    GResponseOut (f :+: g)                        = Either (GResponseOut f) (GResponseOut g)


-- | Computes the curried-codec argument list for 'responsesOf'.
type family GResponseFunc (f :: Type -> Type) (r :: Type) :: Type where
    GResponseFunc (D1 m f)                             r = GResponseFunc f r
    GResponseFunc (C1 m f)                             r = GResponseFunc f r
    GResponseFunc (S1 m (Rec0 (Response Value s h b))) r = Response IsoCodec s h b -> r
    GResponseFunc (f :+: g)                            r = GResponseFunc f (GResponseFunc g r)


-- | The iso between a Generic Rep and its nested-coproduct view — the
--   introduction ('gResponseTo') and elimination ('gResponseFrom') of the
--   "generic case" representation. Mode-polymorphic over the Rep.
class GResponseOps (f :: Type -> Type) where
    gResponseTo   :: Proxy f -> GResponseOut f -> f ()
    gResponseFrom :: f () -> GResponseOut f

runGResponseTo :: forall f. GResponseOps f => GResponseOut f -> f ()
runGResponseTo = gResponseTo (Proxy @f)

runGResponseFrom :: forall f. GResponseOps f => f () -> GResponseOut f
runGResponseFrom = gResponseFrom

instance GResponseOps f => GResponseOps (D1 m f) where
    gResponseTo _ x       = M1 (runGResponseTo @f x)
    gResponseFrom (M1 x)  = runGResponseFrom @f x

instance GResponseOps f => GResponseOps (C1 m f) where
    gResponseTo _ x       = M1 (runGResponseTo @f x)
    gResponseFrom (M1 x)  = runGResponseFrom @f x

instance GResponseOps (S1 m (Rec0 (Response g s h b))) where
    gResponseTo _ x           = M1 (K1 x)
    gResponseFrom (M1 (K1 x)) = x

instance (GResponseOps f, GResponseOps g) => GResponseOps (f :+: g) where
    gResponseTo _ (Left  x) = L1 (runGResponseTo @f x)
    gResponseTo _ (Right x) = R1 (runGResponseTo @g x)
    gResponseFrom (L1 x)    = Left  (runGResponseFrom @f x)
    gResponseFrom (R1 x)    = Right (runGResponseFrom @g x)


-- | Reconstructs an HKD response value from its nested-coproduct view.
--   Mode-polymorphic: @buildR \@Value@ for success, @buildR \@ParseError@ for
--   errors — the single piece of "wrapping" machinery, shared across modes.
buildR :: forall f r. (Generic (r f), GResponseOps (Rep (r f))) => GResponseOut (Rep (r f)) -> r f
buildR = to . runGResponseTo @(Rep (r f))


-- | Builds the two-index 'Responses' tree from one 'Response' codec per
--   constructor. @f@ = @Rep (r Value)@ drives the codec arguments; @g@ =
--   @Rep (r ParseError)@ pins the @ParseError@ Either-tree index.
class (GResponseOps f, GResponseOps g) => GBuildResponses (f :: Type -> Type) (g :: Type -> Type) where
    gBuild :: forall r res. Proxy f -> Proxy g -> (Responses r (GResponseOut g) (GResponseOut f) -> res) -> GResponseFunc f res

runGBuild ::
    forall f g r res. GBuildResponses f g =>
    (Responses r (GResponseOut g) (GResponseOut f) -> res) -> GResponseFunc f res
runGBuild = gBuild (Proxy @f) (Proxy @g)

instance GBuildResponses f g => GBuildResponses (D1 m f) (D1 m' g) where
    gBuild _ _ k = runGBuild @f @g k

instance GBuildResponses f g => GBuildResponses (C1 m f) (C1 m' g) where
    gBuild _ _ k = runGBuild @f @g k

instance GBuildResponses
    (S1 m  (Rec0 (Response Value s h b)))
    (S1 m' (Rec0 (Response ParseError s h b))) where
    gBuild _ _ k codec = k (Only codec)

instance (GBuildResponses f1 g1, GBuildResponses f2 g2) => GBuildResponses (f1 :+: f2) (g1 :+: g2) where
    gBuild _ _ k =
        runGBuild @f1 @g1 $ \tl ->
        runGBuild @f2 @g2 $ \tr ->
        k (Choice tl tr)


-- | Class for HKD sum response types; derive with @Generic@ to get 'responsesOf' for free.
class
    ( Generic (r Value),      GResponseOps (Rep (r Value))
    , Generic (r ParseError), GResponseOps (Rep (r ParseError))
    , GBuildResponses (Rep (r Value)) (Rep (r ParseError))
    ) =>
    ResponseEnum (r :: ((Type -> Type) -> Type -> Type) -> Type)
    where
    -- | Build the response tree by supplying one 'Response' codec per constructor, in order.
    responsesOf ::
        GResponseFunc (Rep (r Value))
            (Responses r (GResponseOut (Rep (r ParseError))) (GResponseOut (Rep (r Value))))
    responsesOf = runGBuild @(Rep (r Value)) @(Rep (r ParseError)) @r id
