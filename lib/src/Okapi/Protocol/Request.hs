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
import Data.Text qualified as Text
import GHC.Generics (Generic, Rep)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec, ForRequest, IsoCodec (..), Value (..))
import Okapi.Protocol.Body (Body)
import Okapi.Protocol.Body qualified as Body
import Okapi.Protocol.Headers (Headers, GHeaders, headersCodec)
import Okapi.Protocol.Headers qualified as Headers
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

requestValue :: method -> path -> query -> headers -> IO body -> Request Value method path query headers body
requestValue m p q h b = Request
    { method  = Value m
    , path    = Value p
    , query   = Value q
    , headers = Value h
    , body    = Value b
    }

request :: Request IsoCodec HTTP.Method [Text.Text] HTTP.Query [HTTP.Header] LBS.ByteString
request = Request
    { method  = IsoCodec Method.raw
    , path    = IsoCodec Path.raw
    , query   = IsoCodec Query.raw
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

methodGET :: Request IsoCodec GET [Text.Text] HTTP.Query [HTTP.Header] LBS.ByteString
methodGET = request & method GET

methodPOST :: Request IsoCodec POST [Text.Text] HTTP.Query [HTTP.Header] LBS.ByteString
methodPOST = request & method POST

methodDELETE :: Request IsoCodec DELETE [Text.Text] HTTP.Query [HTTP.Header] LBS.ByteString
methodDELETE = request & method DELETE

method ::
  KnownMethod meth ->
  Request IsoCodec HTTP.Method path query headers body ->
  Request IsoCodec (KnownMethod meth) path query headers body
method km r = r { method = IsoCodec (Method.method km) }

path ::
  Codec Path path path ->
  Request IsoCodec meth [Text.Text] query headers body ->
  Request IsoCodec meth path query headers body
path c r = r { path = IsoCodec c }

query ::
  Codec Query query query ->
  Request IsoCodec meth path HTTP.Query headers body ->
  Request IsoCodec meth path query headers body
query c r = r { query = IsoCodec c }

headers ::
  Codec (Headers ForRequest) h h ->
  Request IsoCodec meth path query [HTTP.Header] bdy ->
  Request IsoCodec meth path query h bdy
headers c r = r { headers = IsoCodec c }

body ::
  Codec (Body ForRequest) (IO b) (IO b) ->
  Request IsoCodec meth path query hs LBS.ByteString ->
  Request IsoCodec meth path query hs b
body c r = r { body = IsoCodec c }

pathOf :: forall a meth q h b. (Generic a, GPath (Rep a))
       => Request IsoCodec meth [Text.Text] q h b -> Request IsoCodec meth a q h b
pathOf r = r { path = IsoCodec (Path.pathCodec @a) }

queryOf :: forall a meth p h b. (Generic a, GQuery (Rep a))
        => Request IsoCodec meth p HTTP.Query h b -> Request IsoCodec meth p a h b
queryOf r = r { query = IsoCodec (Query.queryCodec @a) }

headersOf :: forall a meth p q b. (Generic a, GHeaders ForRequest (Rep a))
          => Request IsoCodec meth p q [HTTP.Header] b -> Request IsoCodec meth p q a b
headersOf r = r { headers = IsoCodec (headersCodec @ForRequest @a) }

