{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Response codec for a sum of alternatives.
--
--   The codec is a 'NonEmpty' list of the response sum at @IsoCodec@ mode —
--   one element per constructor, each holding that constructor's
--   'Response' codec. This mirrors @Request IsoCodec@ (the HKD at codec mode);
--   a response is a /sum/, so its codec is the list of all its branches.
--
--   Every response operation is "unwrap the inhabited 'Response', do a thing,
--   rewrap the same constructor". Because the operations are polymorphic over
--   the sum type @r@ (no constructor names available), that unwrap/rewrap is a
--   GHC.Generics traversal. Two primitives capture it: 'traverseResponse'
--   (unary) and 'zipResponse' (binary, for printing). Everything else —
--   parsing, the @Result@→@Value@/@ParseError@ splits, OpenApi — is a plain
--   function built on top.
module Okapi.Responses
    ( Responses (Responses)
    , GResponseFunc
    , GBuildList (..)
    , GTraverse (..)
    , GZip (..)
    , traverseResponse
    , zipResponse
    , ResponseEnum (..)
    , parseResponseResult
    , resultToValue
    , resultToParseError
    , printOne
    , extractWaiResBody
    ) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, Rep, S1, (:+:) (..))
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Body qualified as Body
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Headers qualified as Headers
import Okapi.Response (Response)
import Okapi.Response qualified as OkRes
import Okapi.Response.Status qualified as Status


-- | The response codec: a non-empty list of per-constructor codecs (the sum at
--   @IsoCodec@ mode). The constructor is not exported beyond the library, so
--   'responsesOf' is the only way to build one — guaranteeing exactly one codec
--   per constructor, in order.
newtype Responses (r :: ((Type -> Type) -> Type -> Type) -> Type) = Responses (NonEmpty (r IsoCodec))


-- | The curried argument list for 'responsesOf': one @Response IsoCodec@ per
--   constructor of @r@, in declaration order.
type family GResponseFunc (f :: Type -> Type) (res :: Type) :: Type where
    GResponseFunc (D1 m f)                               res = GResponseFunc f res
    GResponseFunc (C1 m f)                               res = GResponseFunc f res
    GResponseFunc (S1 m (Rec0 (Response IsoCodec s h b))) res = Response IsoCodec s h b -> res
    GResponseFunc (f :+: g)                              res = GResponseFunc f (GResponseFunc g res)


-- ── traverseResponse: the unary primitive ────────────────────────────────────

-- | Generic "apply a mode-changing action to the inhabited 'Response', rewrap
--   the same constructor". @f@/@g@ are the modes, @fi@/@fg@ the matching Reps.
class GTraverse
        (f :: (Type -> Type) -> Type -> Type)
        (g :: (Type -> Type) -> Type -> Type)
        (fi :: Type -> Type)
        (fg :: Type -> Type) where
    gtraverse :: Functor t => (forall s h b. Response f s h b -> t (Response g s h b)) -> fi () -> t (fg ())

instance GTraverse f g fi fg => GTraverse f g (D1 m fi) (D1 m fg) where
    gtraverse k (M1 x) = M1 <$> gtraverse k x

instance GTraverse f g fi fg => GTraverse f g (C1 m fi) (C1 m fg) where
    gtraverse k (M1 x) = M1 <$> gtraverse k x

instance GTraverse f g (S1 m (Rec0 (Response f s h b))) (S1 m (Rec0 (Response g s h b))) where
    gtraverse k (M1 (K1 r)) = M1 . K1 <$> k r

instance (GTraverse f g fil fgl, GTraverse f g fir fgr)
    => GTraverse f g (fil :+: fir) (fgl :+: fgr) where
    gtraverse k (L1 x) = L1 <$> gtraverse k x
    gtraverse k (R1 x) = R1 <$> gtraverse k x

-- | Unwrap the inhabited 'Response' of an @r f@, run an action that changes its
--   mode to @g@, rewrap the same constructor. (@Functor@ suffices: each
--   constructor has exactly one 'Response' field.)
traverseResponse ::
    forall f g r t.
    (Generic (r f), Generic (r g), GTraverse f g (Rep (r f)) (Rep (r g)), Functor t) =>
    (forall s h b. Response f s h b -> t (Response g s h b)) -> r f -> t (r g)
traverseResponse k = fmap to . gtraverse k . from


-- ── zipResponse: the binary primitive (printing) ─────────────────────────────

-- | Generic "if these two values are the same constructor, hand me both their
--   'Response's". Used to pair an @r Value@ with its codec from the list.
class GZip
        (f :: (Type -> Type) -> Type -> Type)
        (g :: (Type -> Type) -> Type -> Type)
        (fi :: Type -> Type)
        (fg :: Type -> Type) where
    gzip :: (forall s h b. Response f s h b -> Response g s h b -> c) -> fi () -> fg () -> Maybe c

instance GZip f g fi fg => GZip f g (D1 m fi) (D1 m fg) where
    gzip k (M1 a) (M1 b) = gzip k a b

instance GZip f g fi fg => GZip f g (C1 m fi) (C1 m fg) where
    gzip k (M1 a) (M1 b) = gzip k a b

instance GZip f g (S1 m (Rec0 (Response f s h b))) (S1 m (Rec0 (Response g s h b))) where
    gzip k (M1 (K1 a)) (M1 (K1 b)) = Just (k a b)

instance (GZip f g fil fgl, GZip f g fir fgr)
    => GZip f g (fil :+: fir) (fgl :+: fgr) where
    gzip k (L1 a) (L1 b) = gzip k a b
    gzip k (R1 a) (R1 b) = gzip k a b
    gzip _ _      _      = Nothing

-- | Pair two HKD response values when they inhabit the same constructor.
zipResponse ::
    forall f g r c.
    (Generic (r f), Generic (r g), GZip f g (Rep (r f)) (Rep (r g))) =>
    (forall s h b. Response f s h b -> Response g s h b -> c) -> r f -> r g -> Maybe c
zipResponse k a b = gzip k (from a) (from b)


-- ── GBuildList: build the codec list from the variadic codecs ─────────────────

-- | Walks @Rep (r IsoCodec)@, injecting each supplied codec into its constructor
--   and collecting the results into a 'NonEmpty'.
class GBuildList (f :: Type -> Type) where
    gBuildList ::
        forall r res. Generic (r IsoCodec) =>
        (f () -> Rep (r IsoCodec) ()) -> (NonEmpty (r IsoCodec) -> res) -> GResponseFunc f res

instance GBuildList fi => GBuildList (D1 m fi) where
    gBuildList path cont = gBuildList (path . M1) cont

instance GBuildList fi => GBuildList (C1 m fi) where
    gBuildList path cont = gBuildList (path . M1) cont

instance GBuildList (S1 m (Rec0 (Response IsoCodec s h b))) where
    gBuildList path cont codec = cont (to (path (M1 (K1 codec))) :| [])

instance (GBuildList fil, GBuildList fir) => GBuildList (fil :+: fir) where
    gBuildList path cont =
        gBuildList (path . L1) $ \ls ->
        gBuildList (path . R1) $ \rs ->
        cont (ls <> rs)


-- ── leaf helpers (single-Response parse/print; reused by the primitives) ──────

extractWaiResBody :: Wai.Response -> LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = Builder.toLazyByteString b
extractWaiResBody _                            = LBS.empty

-- | Parse a single 'Response' codec against a 'Wai.Response', per-field 'Result'.
parseResponseResult :: Response IsoCodec s h b -> Wai.Response -> IO (Response Result s h b)
parseResponseResult res waiRes = do
    let status  = Wai.responseStatus  waiRes
        hdrs    = Wai.responseHeaders waiRes
        bodyLbs = extractWaiResBody   waiRes
        (sr, _) = Status.parse  res.status.isoCodec  status
        (hr, _) = Headers.parse res.headers.isoCodec hdrs
        (br, _) = Body.parse    res.body.isoCodec    bodyLbs
    pure OkRes.Response
        { status  = Result sr
        , headers = Result hr
        , body    = Result br
        }

-- | Project a parsed 'Result' response into 'Value' mode; succeeds iff every field parsed.
resultToValue :: Response Result s h b -> Maybe (Response Value s h b)
resultToValue r = case (r.status.result, r.headers.result, r.body.result) of
    (Right s, Right h, Right b) -> Just (OkRes.response s h b)
    _                           -> Nothing

-- | Project a parsed 'Result' response into its per-field 'ParseError' view.
resultToParseError :: Response Result s h b -> Response ParseError s h b
resultToParseError r = OkRes.Response
    { status  = ParseError (either Just (const Nothing) r.status.result)
    , headers = ParseError (either Just (const Nothing) r.headers.result)
    , body    = ParseError (either Just (const Nothing) r.body.result)
    }

-- | Serialize a single 'Response' value with its codec.
printOne :: Response IsoCodec s h b -> Response Value s h b -> IO Wai.Response
printOne res rv = do
    bodyBytes <- Body.printM res.body.isoCodec rv.body.value
    pure (Wai.responseLBS
        (Status.print  res.status.isoCodec  rv.status.value)
        (Headers.print res.headers.isoCodec rv.headers.value)
        bodyBytes)


-- ── ResponseEnum ──────────────────────────────────────────────────────────────

-- | Class for HKD sum response types; derive with @Generic@ to get 'responsesOf' for free.
class
    ( Generic (r IsoCodec), Generic (r Result), Generic (r Value), Generic (r ParseError)
    , GBuildList (Rep (r IsoCodec))
    , GTraverse IsoCodec Result     (Rep (r IsoCodec)) (Rep (r Result))
    , GTraverse Result   Value      (Rep (r Result))   (Rep (r Value))
    , GTraverse Result   ParseError (Rep (r Result))   (Rep (r ParseError))
    , GTraverse IsoCodec IsoCodec   (Rep (r IsoCodec)) (Rep (r IsoCodec))
    , GZip      IsoCodec Value      (Rep (r IsoCodec)) (Rep (r Value))
    ) =>
    ResponseEnum (r :: ((Type -> Type) -> Type -> Type) -> Type)
    where
    -- | Build the response codec by supplying one 'Response' codec per constructor, in order.
    responsesOf :: GResponseFunc (Rep (r IsoCodec)) (Responses r)
    responsesOf = gBuildList id (Responses @r)
