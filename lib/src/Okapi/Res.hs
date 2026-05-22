module Okapi.Res where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec, IsoCodec, Value (..))
import Okapi.Kind (STATUS (..))
import Okapi.Res.Body (ResBody)
import Okapi.Res.Headers (ResHeaders)
import Okapi.Res.Status (KnownStatus, Status)

data Res (f :: (Type -> Type) -> Type -> Type) s h b = Res
  { status_ :: f Status s
  , resHeaders_ :: f ResHeaders h
  , resBody_ :: f ResBody (IO b)
  }

resValue :: s -> h -> IO b -> Res Value s h b
resValue s h b = Res
    { status_     = Value s
    , resHeaders_ = Value h
    , resBody_    = Value b
    }

res :: Res IsoCodec HTTP.Status HTTP.ResponseHeaders LBS.ByteString
res = undefined

ok :: Res IsoCodec (KnownStatus S200) HTTP.ResponseHeaders LBS.ByteString
ok = undefined

notFound :: Res IsoCodec (KnownStatus S404) HTTP.ResponseHeaders LBS.ByteString
notFound = undefined

serverError :: Res IsoCodec (KnownStatus S500) HTTP.ResponseHeaders LBS.ByteString
serverError = undefined

resHeaders ::
  Codec ResHeaders h h ->
  ( Res IsoCodec s HTTP.ResponseHeaders b ->
    Res IsoCodec s h b
  )
resHeaders = undefined

resBody ::
  Codec ResBody b b ->
  ( Res IsoCodec s h LBS.ByteString ->
    Res IsoCodec s h b
  )
resBody = undefined

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a)

json ::
  forall s h b.
  (IsoJson b) =>
  ( Res IsoCodec s h LBS.ByteString ->
    Res IsoCodec s h b
  )
json = undefined
