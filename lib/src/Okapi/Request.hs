{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
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
import Okapi.Body (Body, ForRequest, HasBody (..))
import Okapi.Body qualified as Body
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Data (FromPathData, FromQueryData, ToPathData, ToQueryData)
import Okapi.Headers (HasHeaders (..), Headers)
import Okapi.Headers qualified as Headers
import Okapi.Request.Method (KnownMethod (..), Method, GET, POST, DELETE)
import Okapi.Request.Method qualified as Method
import Okapi.Request.Path (Path)
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query (Query)
import Okapi.Request.Query qualified as Query

data Request (f :: (Type -> Type) -> Type -> Type) m p q h b = Request
  { method  :: f Method m
  , path    :: f Path p
  , query   :: f Query q
  , headers :: f (Headers ForRequest) h
  , body    :: f (Body ForRequest) (IO b)
  }

request :: m -> p -> q -> h -> IO b -> Request Value m p q h b
request m p q h b = Request
    { method  = Value m
    , path    = Value p
    , query   = Value q
    , headers = Value h
    , body    = Value b
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
    { method  = IsoCodec Method.raw
    , path    = IsoCodec Path.raw
    , query   = IsoCodec Query.raw
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
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
method km r = r { method = IsoCodec (Method.method km) }

path ::
  Codec Path p p ->
  ( Request IsoCodec m [Text.Text] q h b ->
    Request IsoCodec m p q h b
  )
path c r = r { path = IsoCodec c }

query ::
  Codec Query q q ->
  ( Request IsoCodec m p HTTP.Query h b ->
    Request IsoCodec m p q h b
  )
query c r = r { query = IsoCodec c }

instance HasHeaders (Request IsoCodec m p q) where
    type Ctx (Request IsoCodec m p q) = ForRequest
    headers c r = r { headers = IsoCodec c }

instance HasBody (Request IsoCodec m p q) where
    type BodyCtx (Request IsoCodec m p q) = ForRequest
    body c r = r { body = IsoCodec c }

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
