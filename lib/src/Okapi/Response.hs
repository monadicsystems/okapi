{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Response where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Body (Body, ForResponse, HasBody (..))
import Okapi.Body qualified as Body
import Okapi.Codec (IsoCodec (..), Value (..))
import Okapi.Headers (HasHeaders (..), Headers)
import Okapi.Headers qualified as Headers
import Okapi.Response.Status (Status, S200, S201, S204, S404, S500)
import Okapi.Response.Status qualified as Status

data Response (f :: (Type -> Type) -> Type -> Type) s h b = Response
  { status  :: f Status s
  , headers :: f (Headers ForResponse) h
  , body    :: f (Body ForResponse) (IO b)
  }

-- | Construct a response value for use inside a handler.
response :: s -> h -> IO b -> Response Value s h b
response s h b = Response
    { status  = Value s
    , headers = Value h
    , body    = Value b
    }

-- | Response codec starting at HTTP 200, raw headers, and raw body.
s200 :: Response IsoCodec S200 [HTTP.Header] LBS.ByteString
s200 = Response
    { status  = IsoCodec (Status.status Status.S200)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 201, raw headers, and raw body.
s201 :: Response IsoCodec S201 [HTTP.Header] LBS.ByteString
s201 = Response
    { status  = IsoCodec (Status.status Status.S201)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 204, raw headers, and raw body.
s204 :: Response IsoCodec S204 [HTTP.Header] LBS.ByteString
s204 = Response
    { status  = IsoCodec (Status.status Status.S204)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 404, raw headers, and raw body.
s404 :: Response IsoCodec S404 [HTTP.Header] LBS.ByteString
s404 = Response
    { status  = IsoCodec (Status.status Status.S404)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec starting at HTTP 500, raw headers, and raw body.
s500 :: Response IsoCodec S500 [HTTP.Header] LBS.ByteString
s500 = Response
    { status  = IsoCodec (Status.status Status.S500)
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

-- | Response codec accepting any status, raw headers, and raw body.
res :: Response IsoCodec HTTP.Status [HTTP.Header] LBS.ByteString
res = Response
    { status  = IsoCodec Status.raw
    , headers = IsoCodec Headers.raw
    , body    = IsoCodec Body.raw
    }

instance HasHeaders (Response IsoCodec s) where
    type Ctx (Response IsoCodec s) = ForResponse
    headers c r = r { headers = IsoCodec c }

instance HasBody (Response IsoCodec s) where
    type BodyCtx (Response IsoCodec s) = ForResponse
    body c r = r { body = IsoCodec c }
