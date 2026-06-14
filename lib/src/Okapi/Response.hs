{-# LANGUAGE OverloadedStrings #-}

module Okapi.Response where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Data.Typeable (Typeable)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Data (FromCookieData, FromHeaderData, ToCookieData, ToHeaderData)
import Okapi.Response.Body (Body, NoContent (..))
import Okapi.Response.Body qualified as Body
import Okapi.Response.Headers (Headers)
import Okapi.Response.Headers qualified as ResHeaders
import Okapi.Response.Status (Status, S200, S201, S204, S404, S500)
import Okapi.Response.Status qualified as Status

data Res (f :: (Type -> Type) -> Type -> Type) s h b = Res
  { status_  :: f Status s
  , headers_ :: f Headers h
  , body_    :: f Body (IO b)
  }

value :: s -> h -> IO b -> Res Value s h b
value s h b = Res
    { status_  = Value s
    , headers_ = Value h
    , body_    = Value b
    }

s200 :: Res IsoCodec S200 HTTP.ResponseHeaders LBS.ByteString
s200 = Res
    { status_  = IsoCodec (Status.known Status.S200)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

s201 :: Res IsoCodec S201 HTTP.ResponseHeaders LBS.ByteString
s201 = Res
    { status_  = IsoCodec (Status.known Status.S201)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

s204 :: Res IsoCodec S204 HTTP.ResponseHeaders NoContent
s204 = Res
    { status_  = IsoCodec (Status.known Status.S204)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.noContent
    }

s404 :: Res IsoCodec S404 HTTP.ResponseHeaders LBS.ByteString
s404 = Res
    { status_  = IsoCodec (Status.known Status.S404)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

s500 :: Res IsoCodec S500 HTTP.ResponseHeaders LBS.ByteString
s500 = Res
    { status_  = IsoCodec (Status.known Status.S500)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

headers ::
  Codec Headers h h ->
  ( Res IsoCodec s HTTP.ResponseHeaders b ->
    Res IsoCodec s h b
  )
headers c r = r { headers_ = IsoCodec c }

body ::
  Codec Body (IO b) (IO b) ->
  ( Res IsoCodec s h LBS.ByteString ->
    Res IsoCodec s h b
  )
body c r = r { body_ = IsoCodec c }

res :: Res IsoCodec HTTP.Status HTTP.ResponseHeaders LBS.ByteString
res = Res
    { status_  = IsoCodec Status.raw
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

json ::
  forall b s h.
  IsoJson b =>
  ( Res IsoCodec s h LBS.ByteString ->
    Res IsoCodec s h b
  )
json r =
    let IsoCodec hCodec = headers_ r
    in r { body_    = IsoCodec Body.json
         , headers_ = IsoCodec (ResHeaders.withHeader "content-type" "application/json" hCodec)
         }

header :: (Typeable a, ToHeaderData a, FromHeaderData a) => HTTP.HeaderName -> Codec Headers a a
header k = ResHeaders.header k

header' :: (Typeable a, ToHeaderData a, FromHeaderData a) => HTTP.HeaderName -> Codec Headers (Maybe a) (Maybe a)
header' k = ResHeaders.header' k

setCookie :: (Typeable a, ToCookieData a, FromCookieData a) => ByteString -> Codec Headers a a
setCookie name = ResHeaders.setCookie name

setCookie' :: (Typeable a, ToCookieData a, FromCookieData a) => ByteString -> Codec Headers (Maybe a) (Maybe a)
setCookie' name = ResHeaders.setCookie' name
