module Okapi.Res where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (IsoCodec)
import Okapi.Res.Body (ResBody)
import Okapi.Res.Headers (ResHeaders)
import Okapi.Res.Status (Status)

data Res (f :: (Type -> Type) -> Type -> Type) s h b = Res
    { status_ :: f Status s
    , resHeaders_ :: f ResHeaders h
    , resBody_ :: f ResBody b
    }

response :: Res IsoCodec HTTP.Status HTTP.ResponseHeaders LBS.ByteString
response = undefined

status ::
    IsoCodec Status s ->
    ( Res IsoCodec HTTP.Status h b ->
      Res IsoCodec s h b
    )
status = undefined

resHeaders ::
    IsoCodec ResHeaders h ->
    ( Res IsoCodec s HTTP.ResponseHeaders b ->
      Res IsoCodec s h b
    )
resHeaders = undefined

resBody ::
    IsoCodec ResBody b ->
    ( Res IsoCodec s h LBS.ByteString ->
      Res IsoCodec s h b
    )
resBody = undefined
