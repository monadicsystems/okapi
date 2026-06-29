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
import Okapi.Codec (Codec, ForResponse, IsoCodec (..), ParseError (..), Result (..), Value (..))
import Okapi.Protocol.Body (Body)
import Okapi.Protocol.Body qualified as Body
import Okapi.Protocol.Headers (Headers)
import Okapi.Protocol.Headers qualified as Headers
import Okapi.Protocol.Response.Status (Status, S200, S201, S204, S404, S500)
import Okapi.Protocol.Response.Status qualified as Status

data Response (mode :: (Type -> Type) -> Type -> Type) status headers body = Response
    { status  :: mode Status status
    , headers :: mode (Headers ForResponse) headers
    , body    :: mode (Body ForResponse) (IO body)
    }

response :: Response IsoCodec HTTP.Status [HTTP.Header] LBS.ByteString
response = Response
    { status  = IsoCodec Status.raw
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

responseValue :: status -> headers -> IO body -> Response Value status headers body
responseValue s h b = Response
    { status  = Value s
    , headers = Value h
    , body    = Value b
    }

status200 :: Response IsoCodec S200 [HTTP.Header] LBS.ByteString
status200 = Response
    { status  = IsoCodec (Status.status Status.S200)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

status201 :: Response IsoCodec S201 [HTTP.Header] LBS.ByteString
status201 = Response
    { status  = IsoCodec (Status.status Status.S201)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

status204 :: Response IsoCodec S204 [HTTP.Header] LBS.ByteString
status204 = Response
    { status  = IsoCodec (Status.status Status.S204)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

status404 :: Response IsoCodec S404 [HTTP.Header] LBS.ByteString
status404 = Response
    { status  = IsoCodec (Status.status Status.S404)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

status500 :: Response IsoCodec S500 [HTTP.Header] LBS.ByteString
status500 = Response
    { status  = IsoCodec (Status.status Status.S500)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

headers ::
  Codec (Headers ForResponse) h h ->
  Response IsoCodec status [HTTP.Header] bdy ->
  Response IsoCodec status h bdy
headers c r = r { headers = IsoCodec c }

body ::
  Codec (Body ForResponse) (IO b) (IO b) ->
  Response IsoCodec status hs LBS.ByteString ->
  Response IsoCodec status hs b
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
    (Right s, Right h, Right b) -> Just (responseValue s h b)
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
    let hdrs = Headers.print codec.headers.isoCodec rv.headers.value
    pure (Wai.responseLBS
        (Status.print codec.status.isoCodec rv.status.value)
        hdrs
        bodyBytes)

newtype Responses
        (mode :: (Type -> Type) -> Type -> Type)
        (responses :: ((Type -> Type) -> Type -> Type) -> Type)
    = Responses (NonEmpty (responses mode))

getResponses :: Responses mode responses -> NonEmpty (responses mode)
getResponses (Responses xs) = xs

type family GResponseFunc (f :: Type -> Type) (res :: Type) :: Type where
    GResponseFunc (D1 meta f)                                              res = GResponseFunc f res
    GResponseFunc (C1 meta f)                                              res = GResponseFunc f res
    GResponseFunc (S1 meta (Rec0 (Response IsoCodec status headers body))) res = Response IsoCodec status headers body -> res
    GResponseFunc (f :+: g)                                                res = GResponseFunc f (GResponseFunc g res)

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

traverseResponses ::
    forall f g responses t.
    (Generic (responses f), Generic (responses g), GTraverse f g (Rep (responses f)) (Rep (responses g)), Functor t) =>
    (forall status headers body. Response f status headers body -> t (Response g status headers body)) -> responses f -> t (responses g)
traverseResponses k = fmap to . gtraverse k . from

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

zipResponses ::
    forall f g responses c.
    (Generic (responses f), Generic (responses g), GZip f g (Rep (responses f)) (Rep (responses g))) =>
    (forall status headers body. Response f status headers body -> Response g status headers body -> c) -> responses f -> responses g -> Maybe c
zipResponses k a b = gzip k (from a) (from b)

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

    cases :: GResponseFunc (Rep (responses IsoCodec)) (Responses IsoCodec responses)
    cases = gConstruct id (Responses @IsoCodec @responses)

