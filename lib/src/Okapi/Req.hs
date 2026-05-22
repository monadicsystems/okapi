module Okapi.Req where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Kind qualified as Kind
import Okapi.Req.Body (Body)
import Okapi.Req.Headers (Headers)
import Okapi.Req.Method (KnownMethod (..), Method)
import Okapi.Req.Path (Path)
import Okapi.Req.Query (Query)

data Req (f :: (Type -> Type) -> Type -> Type) m p q h b = Req
  { method_     :: f Method m
  , path_       :: f Path p
  , query_      :: f Query q
  , headers_ :: f Headers h
  , body_    :: f Body (IO b)
  }

value :: m -> p -> q -> h -> IO b -> Req Value m p q h b
value m p q h b = Req
    { method_     = Value m
    , path_       = Value p
    , query_      = Value q
    , headers_ = Value h
    , body_    = Value b
    }

req ::
  Req
    IsoCodec
    HTTP.Method
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
req = undefined

get ::
  Req
    IsoCodec
    (KnownMethod Kind.GET)
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
get = req & method GET

post ::
  Req
    IsoCodec
    (KnownMethod Kind.POST)
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
post = req & method POST

delete ::
  Req
    IsoCodec
    (KnownMethod Kind.DELETE)
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
delete = req & method DELETE

method ::
  KnownMethod m ->
  Req IsoCodec HTTP.Method p q h b ->
  Req IsoCodec (KnownMethod m) p q h b
method = undefined

stdMethod ::
  Req IsoCodec HTTP.Method p q h b ->
  Req IsoCodec HTTP.StdMethod p q h b
stdMethod = undefined

path ::
  Codec Path p p ->
  ( Req IsoCodec m [Text.Text] q h b ->
    Req IsoCodec m p q h b
  )
path = undefined

query ::
  Codec Query q q ->
  ( Req IsoCodec m p HTTP.Query h b ->
    Req IsoCodec m p q h b
  )
query = undefined

headers ::
  Codec Headers h h ->
  ( Req IsoCodec m p q HTTP.RequestHeaders b ->
    Req IsoCodec m p q h b
  )
headers = undefined

body ::
  Codec Body b b ->
  ( Req IsoCodec m p q h LBS.ByteString ->
    Req IsoCodec m p q h b
  )
body = undefined

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a)

json ::
  forall m p q h b.
  (IsoJson b) =>
  ( Req IsoCodec m p q h LBS.ByteString ->
    Req IsoCodec m p q h b
  )
json = undefined
