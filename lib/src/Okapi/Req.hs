module Okapi.Req where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Req.Body (Body)
import Okapi.Req.Body qualified as Body
import Okapi.Req.Headers (Headers)
import Okapi.Req.Headers qualified as Headers
import Okapi.Req.Method (KnownMethod (..), Method, known, KGET, KPOST, KDELETE)
import Okapi.Req.Method qualified as Method
import Okapi.Req.Path (Path)
import Okapi.Req.Path qualified as Path
import Okapi.Req.Query (Query)
import Okapi.Req.Query qualified as Query

data Req (f :: (Type -> Type) -> Type -> Type) m p q h b = Req
  { method_ :: f Method m
  , path_   :: f Path p
  , query_  :: f Query q
  , headers_ :: f Headers h
  , body_    :: f Body (IO b)
  }

value :: m -> p -> q -> h -> IO b -> Req Value m p q h b
value m p q h b = Req
    { method_  = Value m
    , path_    = Value p
    , query_   = Value q
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
req = Req
    { method_  = IsoCodec Method.raw
    , path_    = IsoCodec Path.raw
    , query_   = IsoCodec Query.raw
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

get ::
  Req
    IsoCodec
    KGET
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
get = req & method GET

post ::
  Req
    IsoCodec
    KPOST
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
post = req & method POST

delete ::
  Req
    IsoCodec
    KDELETE
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
delete = req & method DELETE

method ::
  KnownMethod m ->
  Req IsoCodec HTTP.Method p q h b ->
  Req IsoCodec (KnownMethod m) p q h b
method km r = r { method_ = IsoCodec (known km) }

stdMethod ::
  Req IsoCodec HTTP.Method p q h b ->
  Req IsoCodec HTTP.StdMethod p q h b
stdMethod = undefined

path ::
  Codec Path p p ->
  ( Req IsoCodec m [Text.Text] q h b ->
    Req IsoCodec m p q h b
  )
path c r = r { path_ = IsoCodec c }

query ::
  Codec Query q q ->
  ( Req IsoCodec m p HTTP.Query h b ->
    Req IsoCodec m p q h b
  )
query c r = r { query_ = IsoCodec c }

headers ::
  Codec Headers h h ->
  ( Req IsoCodec m p q HTTP.RequestHeaders b ->
    Req IsoCodec m p q h b
  )
headers c r = r { headers_ = IsoCodec c }

body ::
  Codec Body (IO b) (IO b) ->
  ( Req IsoCodec m p q h LBS.ByteString ->
    Req IsoCodec m p q h b
  )
body c r = r { body_ = IsoCodec c }

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a)

json ::
  forall m p q h b.
  (IsoJson b) =>
  ( Req IsoCodec m p q h LBS.ByteString ->
    Req IsoCodec m p q h b
  )
json = body Body.json
