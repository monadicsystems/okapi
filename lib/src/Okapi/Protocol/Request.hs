{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Request where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Rep)
import Network.HTTP.Types qualified as HTTP
import Okapi.Protocol.Shared.Body (Body, ForRequest, HasBody (..))
import Okapi.Protocol.Shared.Body qualified as Body
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Data (FromPathData, FromQueryData, ToPathData, ToQueryData)
import Okapi.Protocol.Shared.Headers (HasHeaders (..), Headers, GHeaders, headersCodec)
import Okapi.Protocol.Shared.Headers qualified as Headers
import Okapi.Protocol.Request.Method (KnownMethod (..), Method, GET, POST, DELETE)
import Okapi.Protocol.Request.Method qualified as Method
import Okapi.Protocol.Request.Path (Path, GPath)
import Okapi.Protocol.Request.Path qualified as Path
import Okapi.Protocol.Request.Query (Query, GQuery)
import Okapi.Protocol.Request.Query qualified as Query

data Request (mode :: (Type -> Type) -> Type -> Type) method path query headers body = Request
  { method  :: mode Method method
  , path    :: mode Path path
  , query   :: mode Query query
  , headers :: mode (Headers ForRequest) headers
  , body    :: mode (Body ForRequest) (IO body)
  }

request :: method -> path -> query -> headers -> IO body -> Request Value method path query headers body
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
  KnownMethod meth ->
  Request IsoCodec HTTP.Method path query headers body ->
  Request IsoCodec (KnownMethod meth) path query headers body
method km r = r { method = IsoCodec (Method.method km) }

path ::
  Codec Path path path ->
  ( Request IsoCodec meth [Text.Text] query headers body ->
    Request IsoCodec meth path query headers body
  )
path c r = r { path = IsoCodec c }

query ::
  Codec Query query query ->
  ( Request IsoCodec meth path HTTP.Query headers body ->
    Request IsoCodec meth path query headers body
  )
query c r = r { query = IsoCodec c }

instance HasHeaders (Request IsoCodec meth path query) where
    type Ctx (Request IsoCodec meth path query) = ForRequest
    headers c r = r { headers = IsoCodec c }

instance HasBody (Request IsoCodec meth path query) where
    type BodyCtx (Request IsoCodec meth path query) = ForRequest
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

flag' :: Text.Text -> Codec Query (Maybe ()) (Maybe ())
flag' k = Query.flag' k

-- | Build a 'Path' codec from a Generic record type and apply it to a request.
pathOf :: forall a meth q h b. (Generic a, GPath (Rep a))
       => Request IsoCodec meth [Text.Text] q h b -> Request IsoCodec meth a q h b
pathOf r = r { path = IsoCodec (Path.pathCodec @a) }

-- | Build a 'Query' codec from a Generic record type and apply it to a request.
queryOf :: forall a meth p h b. (Generic a, GQuery (Rep a))
        => Request IsoCodec meth p HTTP.Query h b -> Request IsoCodec meth p a h b
queryOf r = r { query = IsoCodec (Query.queryCodec @a) }

-- | Build a 'Headers' codec from a Generic record type and apply it to a request.
headersOf :: forall a meth p q b. (Generic a, GHeaders ForRequest (Rep a))
          => Request IsoCodec meth p q [HTTP.Header] b -> Request IsoCodec meth p q a b
headersOf r = r { headers = IsoCodec (headersCodec @ForRequest @a) }
