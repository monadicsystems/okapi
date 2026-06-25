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

    , Prim (..)
    , numPrim, timePrim
    , intPrim, int16Prim, int32Prim, int64Prim, integerPrim
    , boolPrim, charPrim, floatPrim, doublePrim, scientificPrim, textPrim
    , dayPrim, localTimePrim, utcTimePrim, timeOfDayPrim, uuidPrim

    , int, int16, int32, int64, integer
    , bool, char, float, double, scientific, text
    , day, localTime, utcTime, timeOfDay, uuid
    ) where

import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Time.Format (FormatTime, ParseTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Okapi.Codec (ParseErrorOf, PartOf)
import Text.Read (readMaybe)

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

data Prim a = Prim
    { primDecode :: Text -> Either Text a
    , primEncode :: a -> Text
    , primInfo   :: Info
    }

numPrim :: (Show a, Read a) => Text -> Maybe Text -> Prim a
numPrim ty fmt = Prim
    { primDecode = \t -> maybe (Left ("invalid " <> ty <> ": " <> t)) Right (readMaybe (T.unpack t))
    , primEncode = T.pack . show
    , primInfo   = Info ty fmt
    }

timePrim :: (FormatTime a, ParseTime a) => String -> Maybe Text -> Prim a
timePrim fmtStr fmt = Prim
    { primDecode = \t -> maybe (Left ("invalid " <> tn <> ": " <> t)) Right
                               (parseTimeM True defaultTimeLocale fmtStr (T.unpack t))
    , primEncode = T.pack . formatTime defaultTimeLocale fmtStr
    , primInfo   = Info "string" fmt
    }
  where tn = maybe "time" id fmt

intPrim        :: Prim Int        ; intPrim        = numPrim "integer" Nothing
int16Prim      :: Prim Int16      ; int16Prim      = numPrim "integer" (Just "int32")
int32Prim      :: Prim Int32      ; int32Prim      = numPrim "integer" (Just "int32")
int64Prim      :: Prim Int64      ; int64Prim      = numPrim "integer" (Just "int64")
integerPrim    :: Prim Integer    ; integerPrim    = numPrim "integer" Nothing
floatPrim      :: Prim Float      ; floatPrim      = numPrim "number" (Just "float")
doublePrim     :: Prim Double     ; doublePrim     = numPrim "number" (Just "double")
scientificPrim :: Prim Scientific ; scientificPrim = numPrim "number" Nothing

textPrim :: Prim Text
textPrim = Prim Right id (Info "string" Nothing)

boolPrim :: Prim Bool
boolPrim = Prim dec (\b -> if b then "true" else "false") (Info "boolean" Nothing)
  where
    dec t = case T.toLower t of
        "true"  -> Right True
        "1"     -> Right True
        "false" -> Right False
        "0"     -> Right False
        _       -> Left ("invalid boolean: " <> t)

charPrim :: Prim Char
charPrim = Prim dec T.singleton (Info "string" Nothing)
  where
    dec t = case T.unpack t of
        [c] -> Right c
        _   -> Left ("invalid char: " <> t)

uuidPrim :: Prim UUID
uuidPrim = Prim dec UUID.toText (Info "string" (Just "uuid"))
  where
    dec t = maybe (Left ("invalid uuid: " <> t)) Right (UUID.fromText t)

dayPrim       :: Prim Day       ; dayPrim       = timePrim "%Y-%m-%d" (Just "date")
localTimePrim :: Prim LocalTime ; localTimePrim = timePrim "%Y-%m-%dT%H:%M:%S" (Just "date-time")
utcTimePrim   :: Prim UTCTime   ; utcTimePrim   = timePrim "%Y-%m-%dT%H:%M:%SZ" (Just "date-time")
timeOfDayPrim :: Prim TimeOfDay ; timeOfDayPrim = timePrim "%H:%M:%S" (Just "time")

int        :: Data t Int        => Iso t Int        ; int        = iso
int16      :: Data t Int16      => Iso t Int16      ; int16      = iso
int32      :: Data t Int32      => Iso t Int32      ; int32      = iso
int64      :: Data t Int64      => Iso t Int64      ; int64      = iso
integer    :: Data t Integer    => Iso t Integer    ; integer    = iso
bool       :: Data t Bool       => Iso t Bool       ; bool       = iso
char       :: Data t Char       => Iso t Char       ; char       = iso
float      :: Data t Float      => Iso t Float      ; float      = iso
double     :: Data t Double     => Iso t Double     ; double     = iso
scientific :: Data t Scientific => Iso t Scientific ; scientific = iso
text       :: Data t Text       => Iso t Text       ; text       = iso
day        :: Data t Day        => Iso t Day        ; day        = iso
localTime  :: Data t LocalTime  => Iso t LocalTime  ; localTime  = iso
utcTime    :: Data t UTCTime    => Iso t UTCTime    ; utcTime    = iso
timeOfDay  :: Data t TimeOfDay  => Iso t TimeOfDay  ; timeOfDay  = iso
uuid       :: Data t UUID       => Iso t UUID       ; uuid       = iso
