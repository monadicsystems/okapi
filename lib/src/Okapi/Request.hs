{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Request where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Network.HTTP.Types qualified as HTTP
import Okapi.Body (Body, ForRequest, HasBody (..), IsoJson, NoContent, raw)
import Okapi.Body qualified as Body
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Data (FromCookieData, FromHeaderData, FromPathData, FromQueryData, ToCookieData, ToHeaderData, ToPathData, ToQueryData)
import Okapi.Headers (HasHeaders (..), Headers)
import Okapi.Headers qualified as Headers
import Okapi.Request.Method (KnownMethod (..), Method, known, GET, POST, DELETE)
import Okapi.Request.Method qualified as Method
import Okapi.Request.Path (Path)
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query (Query)
import Okapi.Request.Query qualified as Query

data Request (f :: (Type -> Type) -> Type -> Type) m p q h b = Request
  { method_  :: f Method m
  , path_    :: f Path p
  , query_   :: f Query q
  , headers_ :: f (Headers ForRequest) h
  , body_    :: f (Body ForRequest) (IO b)
  }

request :: m -> p -> q -> h -> IO b -> Request Value m p q h b
request m p q h b = Request
    { method_  = Value m
    , path_    = Value p
    , query_   = Value q
    , headers_ = Value h
    , body_    = Value b
    }

req ::
  Request
    IsoCodec
    HTTP.Method
    [Text.Text]
    HTTP.Query
    [HTTP.Header]
    LBS.ByteString
req = Request
    { method_  = IsoCodec Method.raw
    , path_    = IsoCodec Path.raw
    , query_   = IsoCodec Query.raw
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

mGet ::
  Request
    IsoCodec
    GET
    [Text.Text]
    HTTP.Query
    [HTTP.Header]
    LBS.ByteString
mGet = req & method GET

mPost ::
  Request
    IsoCodec
    POST
    [Text.Text]
    HTTP.Query
    [HTTP.Header]
    LBS.ByteString
mPost = req & method POST

mDelete ::
  Request
    IsoCodec
    DELETE
    [Text.Text]
    HTTP.Query
    [HTTP.Header]
    LBS.ByteString
mDelete = req & method DELETE

method ::
  KnownMethod m ->
  Request IsoCodec HTTP.Method p q h b ->
  Request IsoCodec (KnownMethod m) p q h b
method km r = r { method_ = IsoCodec (known km) }

path ::
  Codec Path p p ->
  ( Request IsoCodec m [Text.Text] q h b ->
    Request IsoCodec m p q h b
  )
path c r = r { path_ = IsoCodec c }

query ::
  Codec Query q q ->
  ( Request IsoCodec m p HTTP.Query h b ->
    Request IsoCodec m p q h b
  )
query c r = r { query_ = IsoCodec c }

instance HasHeaders (Request IsoCodec m p q) where
    type Ctx (Request IsoCodec m p q) = ForRequest
    headers c r = r { headers_ = IsoCodec c }

instance HasBody (Request IsoCodec m p q) where
    type BodyCtx (Request IsoCodec m p q) = ForRequest
    body c r = r { body_ = IsoCodec c }

seg_ :: (Typeable a, ToPathData a, FromPathData a) => a -> Codec Path b ()
seg_ x = Path.seg_ x

seg :: (Typeable a, ToPathData a, FromPathData a) => Text.Text -> Codec Path a a
seg n = Path.seg n

segs :: (Typeable a, ToPathData a, FromPathData a) => Codec Path (NonEmpty a) (NonEmpty a)
segs = Path.segs

param :: (Typeable a, ToQueryData a, FromQueryData a) => Text.Text -> Codec Query a a
param k = Query.param k

param' :: (Typeable a, ToQueryData a, FromQueryData a) => Text.Text -> Codec Query (Maybe a) (Maybe a)
param' k = Query.param' k

flag :: Text.Text -> Codec Query () ()
flag k = Query.flag k

flag' :: Text.Text -> Codec Query Bool Bool
flag' k = Query.flag' k
