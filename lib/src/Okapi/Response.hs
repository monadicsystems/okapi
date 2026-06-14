{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Response where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Body (Body, ForResponse, HasBody (..), NoContent, raw)
import Okapi.Body qualified as Body
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Headers (HasHeaders (..), Headers)
import Okapi.Headers qualified as Headers
import Okapi.Response.Status (Status, S200, S201, S204, S404, S500)
import Okapi.Response.Status qualified as Status

data Response (f :: (Type -> Type) -> Type -> Type) s h b = Response
  { status_  :: f Status s
  , headers_ :: f (Headers ForResponse) h
  , body_    :: f (Body ForResponse) (IO b)
  }

response :: s -> h -> IO b -> Response Value s h b
response s h b = Response
    { status_  = Value s
    , headers_ = Value h
    , body_    = Value b
    }

s200 :: Response IsoCodec S200 [HTTP.Header] LBS.ByteString
s200 = Response
    { status_  = IsoCodec (Status.known Status.S200)
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

s201 :: Response IsoCodec S201 [HTTP.Header] LBS.ByteString
s201 = Response
    { status_  = IsoCodec (Status.known Status.S201)
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

s204 :: Response IsoCodec S204 [HTTP.Header] LBS.ByteString
s204 = Response
    { status_  = IsoCodec (Status.known Status.S204)
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

s404 :: Response IsoCodec S404 [HTTP.Header] LBS.ByteString
s404 = Response
    { status_  = IsoCodec (Status.known Status.S404)
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

s500 :: Response IsoCodec S500 [HTTP.Header] LBS.ByteString
s500 = Response
    { status_  = IsoCodec (Status.known Status.S500)
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

res :: Response IsoCodec HTTP.Status [HTTP.Header] LBS.ByteString
res = Response
    { status_  = IsoCodec Status.raw
    , headers_ = IsoCodec Headers.raw
    , body_    = IsoCodec Body.raw
    }

instance HasHeaders (Response IsoCodec s) where
    type Ctx (Response IsoCodec s) = ForResponse
    headers c r = r { headers_ = IsoCodec c }

instance HasBody (Response IsoCodec s) where
    type BodyCtx (Response IsoCodec s) = ForResponse
    body c r = r { body_ = IsoCodec c }
