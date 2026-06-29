{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Okapi.Data
    ( Iso (..)
    , Info (..)
    , Data (..)

    , int, int16, int32, int64, integer
    , bool, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    ) where

import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Okapi.Codec (ParseErrorOf, PartOf)

data Info = Info
    { typeName :: Text
    , format   :: Maybe Text
    }
    deriving (Eq, Show)

type Iso :: (Type -> Type) -> Type -> Type
data Iso t a = Iso
    { decode :: PartOf t -> Either (ParseErrorOf t) a
    , encode :: a -> PartOf t
    , info   :: Info
    }

class Data t a where
    iso :: Iso t a

int        :: Data t Int        => Iso t Int        ; int        = iso
int16      :: Data t Int16      => Iso t Int16      ; int16      = iso
int32      :: Data t Int32      => Iso t Int32      ; int32      = iso
int64      :: Data t Int64      => Iso t Int64      ; int64      = iso
integer    :: Data t Integer    => Iso t Integer    ; integer    = iso
bool       :: Data t Bool       => Iso t Bool       ; bool       = iso
float      :: Data t Float      => Iso t Float      ; float      = iso
double     :: Data t Double     => Iso t Double     ; double     = iso
scientific :: Data t Scientific => Iso t Scientific ; scientific = iso
text       :: Data t Text       => Iso t Text       ; text       = iso
day        :: Data t Day        => Iso t Day        ; day        = iso
localTime  :: Data t LocalTime  => Iso t LocalTime  ; localTime  = iso
utcTime    :: Data t UTCTime    => Iso t UTCTime    ; utcTime    = iso
timeOfDay  :: Data t TimeOfDay  => Iso t TimeOfDay  ; timeOfDay  = iso
uuid       :: Data t UUID       => Iso t UUID       ; uuid       = iso
