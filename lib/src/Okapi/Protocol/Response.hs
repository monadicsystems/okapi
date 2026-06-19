{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Response where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, Rep, S1, (:+:) (..))
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiI
import Okapi.Protocol.Shared.Body (Body, ForResponse, HasBody (..))
import Okapi.Protocol.Shared.Body qualified as Body
import Okapi.Codec (IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Protocol.Shared.Headers (HasHeaders (..), Headers)
import Okapi.Protocol.Shared.Headers qualified as Headers
import Okapi.Protocol.Response.Status (Status, S200, S201, S204, S404, S500)
import Okapi.Protocol.Response.Status qualified as Status


-- ── Single response ───────────────────────────────────────────────────────────

data Response (mode :: (Type -> Type) -> Type -> Type) status headers body = Response
  { status  :: mode Status status
  , headers :: mode (Headers ForResponse) headers
  , body    :: mode (Body ForResponse) (IO body)
  }

-- | Construct a response value for use inside a handler.
response :: status -> headers -> IO body -> Response Value status headers body
response s h b = Response
    { status  = Value s
    , headers = Value h
    , body    = Value b
    }

-- | Response codec starting at HTTP 200, raw headers, and raw body.
s200 :: Response IsoCodec S200 [HTTP.Header] LBS.ByteString
s200 = Response
    { status  = IsoCodec (Status.status Status.S200)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 201, raw headers, and raw body.
s201 :: Response IsoCodec S201 [HTTP.Header] LBS.ByteString
s201 = Response
    { status  = IsoCodec (Status.status Status.S201)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 204, raw headers, and raw body.
s204 :: Response IsoCodec S204 [HTTP.Header] LBS.ByteString
s204 = Response
    { status  = IsoCodec (Status.status Status.S204)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 404, raw headers, and raw body.
s404 :: Response IsoCodec S404 [HTTP.Header] LBS.ByteString
s404 = Response
    { status  = IsoCodec (Status.status Status.S404)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 500, raw headers, and raw body.
s500 :: Response IsoCodec S500 [HTTP.Header] LBS.ByteString
s500 = Response
    { status  = IsoCodec (Status.status Status.S500)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec accepting any status, raw headers, and raw body.
res :: Response IsoCodec HTTP.Status [HTTP.Header] LBS.ByteString
res = Response
    { status  = IsoCodec Status.raw
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

instance HasHeaders (Response IsoCodec status) where
    type Ctx (Response IsoCodec status) = ForResponse
    headers c r = r { headers = IsoCodec c }

instance HasBody (Response IsoCodec status) where
    type BodyCtx (Response IsoCodec status) = ForResponse
    body c r = r { body = IsoCodec c }

extractWaiResBody :: Wai.Response -> LBS.ByteString
extractWaiResBody (WaiI.ResponseBuilder _ _ b) = Builder.toLazyByteString b
extractWaiResBody _                            = LBS.empty

parseResponseResult :: Response IsoCodec status headers body -> Wai.Response -> IO (Response Result status headers body)
parseResponseResult codec waiRes = do
    let status  = Wai.responseStatus  waiRes
        hdrs    = Wai.responseHeaders waiRes
        bodyLbs = extractWaiResBody   waiRes
        (sr, _) = Status.parse  codec.status.isoCodec  status
        (hr, _) = Headers.parse codec.headers.isoCodec hdrs
        (br, _) = Body.parse    codec.body.isoCodec    bodyLbs
    pure Response
        { status  = Result sr
        , headers = Result hr
        , body    = Result br
        }

resultToValue :: Response Result status headers body -> Maybe (Response Value status headers body)
resultToValue r = case (r.status.result, r.headers.result, r.body.result) of
    (Right s, Right h, Right b) -> Just (response s h b)
    _                           -> Nothing

resultToParseError :: Response Result status headers body -> Response ParseError status headers body
resultToParseError r = Response
    { status  = ParseError (either Just (const Nothing) r.status.result)
    , headers = ParseError (either Just (const Nothing) r.headers.result)
    , body    = ParseError (either Just (const Nothing) r.body.result)
    }

printOne :: Response IsoCodec status headers body -> Response Value status headers body -> IO Wai.Response
printOne codec rv = do
    bodyBytes <- Body.printM codec.body.isoCodec rv.body.value
    pure (Wai.responseLBS
        (Status.print  codec.status.isoCodec  rv.status.value)
        (Headers.print codec.headers.isoCodec rv.headers.value)
        bodyBytes)


-- ── Response sum (Responses) ──────────────────────────────────────────────────

-- | Response codec for a sum of alternatives.
--
--   The codec is a 'NonEmpty' list of the response sum at @IsoCodec@ mode —
--   one element per constructor, each holding that constructor's
--   'Response' codec. This mirrors @Request IsoCodec@ (the HKD at codec mode);
--   a response is a /sum/, so its codec is the list of all its branches.
--
--   Every response operation is "unwrap the inhabited 'Response', do a thing,
--   rewrap the same constructor". Because the operations are polymorphic over
--   the sum type @responses@ (no constructor names available), that
--   unwrap/rewrap is a GHC.Generics traversal. Two primitives capture it:
--   'traverseResponses' (unary) and 'zipResponses' (binary, for printing).
--   Everything else — parsing, the @Result@→@Value@/@ParseError@ splits,
--   OpenApi — is a plain function built on top.

-- | The response codec: a non-empty list of per-constructor codecs (the sum at
--   @IsoCodec@ mode). The constructor is not exported beyond the library, so
--   'cases' is the only way to build one — guaranteeing exactly one codec
--   per constructor, in order.
newtype Responses
        (mode :: (Type -> Type) -> Type -> Type)
        (responses :: ((Type -> Type) -> Type -> Type) -> Type)
    = Responses (NonEmpty (responses mode))

getResponses :: Responses mode responses -> NonEmpty (responses mode)
getResponses (Responses xs) = xs


-- | The curried argument list for 'cases': one @Response IsoCodec@ per
--   constructor of @responses@, in declaration order.
type family GResponseFunc (f :: Type -> Type) (res :: Type) :: Type where
    GResponseFunc (D1 meta f)                                              res = GResponseFunc f res
    GResponseFunc (C1 meta f)                                              res = GResponseFunc f res
    GResponseFunc (S1 meta (Rec0 (Response IsoCodec status headers body))) res = Response IsoCodec status headers body -> res
    GResponseFunc (f :+: g)                                                res = GResponseFunc f (GResponseFunc g res)


-- ── traverseResponses: the unary primitive ────────────────────────────────────

-- | Generic "apply a mode-changing action to the inhabited 'Response', rewrap
--   the same constructor". @f@/@g@ are the modes, @fi@/@fg@ the matching Reps.
class GTraverse
        (f :: (Type -> Type) -> Type -> Type)
        (g :: (Type -> Type) -> Type -> Type)
        (fi :: Type -> Type)
        (fg :: Type -> Type) where
    gtraverse :: Functor t => (forall status headers body. Response f status headers body -> t (Response g status headers body)) -> fi () -> t (fg ())

instance GTraverse f g fi fg => GTraverse f g (D1 meta fi) (D1 meta fg) where
    gtraverse k (M1 x) = M1 <$> gtraverse k x

instance GTraverse f g fi fg => GTraverse f g (C1 meta fi) (C1 meta fg) where
    gtraverse k (M1 x) = M1 <$> gtraverse k x

instance GTraverse f g (S1 meta (Rec0 (Response f status headers body))) (S1 meta (Rec0 (Response g status headers body))) where
    gtraverse k (M1 (K1 resp)) = M1 . K1 <$> k resp

instance (GTraverse f g fil fgl, GTraverse f g fir fgr)
    => GTraverse f g (fil :+: fir) (fgl :+: fgr) where
    gtraverse k (L1 x) = L1 <$> gtraverse k x
    gtraverse k (R1 x) = R1 <$> gtraverse k x

-- | Unwrap the inhabited 'Response' of a @responses mode@, run an action that
--   changes its mode to @g@, rewrap the same constructor. (@Functor@ suffices:
--   each constructor has exactly one 'Response' field.)
traverseResponses ::
    forall f g responses t.
    (Generic (responses f), Generic (responses g), GTraverse f g (Rep (responses f)) (Rep (responses g)), Functor t) =>
    (forall status headers body. Response f status headers body -> t (Response g status headers body)) -> responses f -> t (responses g)
traverseResponses k = fmap to . gtraverse k . from


-- ── zipResponses: the binary primitive (printing) ─────────────────────────────

-- | Generic "if these two values are the same constructor, hand me both their
--   'Response's". Used to pair a @responses Value@ with its codec from the list.
class GZip
        (f :: (Type -> Type) -> Type -> Type)
        (g :: (Type -> Type) -> Type -> Type)
        (fi :: Type -> Type)
        (fg :: Type -> Type) where
    gzip :: (forall status headers body. Response f status headers body -> Response g status headers body -> c) -> fi () -> fg () -> Maybe c

instance GZip f g fi fg => GZip f g (D1 meta fi) (D1 meta fg) where
    gzip k (M1 a) (M1 b) = gzip k a b

instance GZip f g fi fg => GZip f g (C1 meta fi) (C1 meta fg) where
    gzip k (M1 a) (M1 b) = gzip k a b

instance GZip f g (S1 meta (Rec0 (Response f status headers body))) (S1 meta (Rec0 (Response g status headers body))) where
    gzip k (M1 (K1 a)) (M1 (K1 b)) = Just (k a b)

instance (GZip f g fil fgl, GZip f g fir fgr)
    => GZip f g (fil :+: fir) (fgl :+: fgr) where
    gzip k (L1 a) (L1 b) = gzip k a b
    gzip k (R1 a) (R1 b) = gzip k a b
    gzip _ _      _      = Nothing

-- | Pair two HKD response values when they inhabit the same constructor.
zipResponses ::
    forall f g responses c.
    (Generic (responses f), Generic (responses g), GZip f g (Rep (responses f)) (Rep (responses g))) =>
    (forall status headers body. Response f status headers body -> Response g status headers body -> c) -> responses f -> responses g -> Maybe c
zipResponses k a b = gzip k (from a) (from b)


-- ── GConstruct: build the codec list from the variadic codecs ─────────────────

-- | Walks @Rep (responses IsoCodec)@, injecting each supplied codec into its
--   constructor and collecting the results into a 'NonEmpty'.
class GConstruct (f :: Type -> Type) where
    gConstruct ::
        forall responses res. Generic (responses IsoCodec) =>
        (f () -> Rep (responses IsoCodec) ()) -> (NonEmpty (responses IsoCodec) -> res) -> GResponseFunc f res

instance GConstruct fi => GConstruct (D1 meta fi) where
    gConstruct inject cont = gConstruct (inject . M1) cont

instance GConstruct fi => GConstruct (C1 meta fi) where
    gConstruct inject cont = gConstruct (inject . M1) cont

instance GConstruct (S1 meta (Rec0 (Response IsoCodec status headers body))) where
    gConstruct inject cont codec = cont (to (inject (M1 (K1 codec))) :| [])

instance (GConstruct fil, GConstruct fir) => GConstruct (fil :+: fir) where
    gConstruct inject cont =
        gConstruct (inject . L1) $ \ls ->
        gConstruct (inject . R1) $ \rs ->
        cont (ls <> rs)


-- ── Cases ─────────────────────────────────────────────────────────────────────

-- | Class for HKD sum response types; derive with @Generic@ to get 'cases' for free.
class
    ( Generic (responses IsoCodec), Generic (responses Result), Generic (responses Value), Generic (responses ParseError)
    , GConstruct (Rep (responses IsoCodec))
    , GTraverse IsoCodec Result     (Rep (responses IsoCodec)) (Rep (responses Result))
    , GTraverse Result   Value      (Rep (responses Result))   (Rep (responses Value))
    , GTraverse Result   ParseError (Rep (responses Result))   (Rep (responses ParseError))
    , GTraverse IsoCodec IsoCodec   (Rep (responses IsoCodec)) (Rep (responses IsoCodec))
    , GZip      IsoCodec Value      (Rep (responses IsoCodec)) (Rep (responses Value))
    ) =>
    Cases (responses :: ((Type -> Type) -> Type -> Type) -> Type)
    where
    -- | Build the response codec by supplying one 'Response' codec per constructor, in order.
    cases :: GResponseFunc (Rep (responses IsoCodec)) (Responses IsoCodec responses)
    cases = gConstruct id (Responses @IsoCodec @responses)
