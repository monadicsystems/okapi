module Okapi.Req where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (IsoCodec)
import Okapi.Kind qualified as Kind
import Okapi.Req.Body (ReqBody)
import Okapi.Req.Headers (ReqHeaders)
import Okapi.Req.Method (KnownMethod (..), Method (..))
import Okapi.Req.Path (Path)
import Okapi.Req.Query (Query)

data Req (f :: (Type -> Type) -> Type -> Type) m p q h b = Req
  { method_ :: f Method m
  , path_ :: f Path p
  , query_ :: f Query q
  , reqHeaders_ :: f ReqHeaders h
  , reqBody_ :: f ReqBody b
  }

request ::
  Req
    IsoCodec
    HTTP.Method
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
request = undefined

get ::
  Req
    IsoCodec
    (KnownMethod Kind.GET)
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
get = request & method GET

post ::
  Req
    IsoCodec
    (KnownMethod Kind.POST)
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
post = request & method POST

delete ::
  Req
    IsoCodec
    (KnownMethod Kind.DELETE)
    [Text.Text]
    HTTP.Query
    HTTP.RequestHeaders
    LBS.ByteString
delete = request & method DELETE

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
  IsoCodec Path p ->
  ( Req IsoCodec m [Text.Text] q h b ->
    Req IsoCodec m p q h b
  )
path = undefined

query ::
  IsoCodec Query q ->
  ( Req IsoCodec m p HTTP.Query h b ->
    Req IsoCodec m p q h b
  )
query = undefined

reqHeaders ::
  IsoCodec ReqHeaders h ->
  ( Req IsoCodec m p q HTTP.RequestHeaders b ->
    Req IsoCodec m p q h b
  )
reqHeaders = undefined

reqBody ::
  IsoCodec ReqBody b ->
  ( Req IsoCodec m p q h LBS.ByteString ->
    Req IsoCodec m p q h b
  )
reqBody = undefined
