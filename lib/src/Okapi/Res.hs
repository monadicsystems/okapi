module Okapi.Res where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec, IsoCodec (..), Value (..))
import Okapi.Res.Body (Body)
import Okapi.Res.Body qualified as Body
import Okapi.Res.Headers (Headers)
import Okapi.Res.Headers qualified as ResHeaders
import Okapi.Res.Status (Status, KS200, KS404, KS500)
import Okapi.Res.Status qualified as Status

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

ok :: Res IsoCodec KS200 HTTP.ResponseHeaders LBS.ByteString
ok = Res
    { status_  = IsoCodec (Status.known Status.S200)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

notFound :: Res IsoCodec KS404 HTTP.ResponseHeaders LBS.ByteString
notFound = Res
    { status_  = IsoCodec (Status.known Status.S404)
    , headers_ = IsoCodec ResHeaders.raw
    , body_    = IsoCodec Body.raw
    }

serverError :: Res IsoCodec KS500 HTTP.ResponseHeaders LBS.ByteString
serverError = Res
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

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a)

json ::
  (IsoJson b) =>
  ( Res IsoCodec s h LBS.ByteString ->
    Res IsoCodec s h b
  )
json = undefined
