{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Cookies (
    Cookie,
    ParseError (..),
    cookieIso,
) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Okapi.Codec (ParseErrorOf, PartOf)
import Okapi.Data
    ( Data (..), Info (..), Iso (..), Prim (..)
    , boolPrim, charPrim, dayPrim, doublePrim, floatPrim
    , int16Prim, int32Prim, int64Prim, intPrim, integerPrim
    , localTimePrim, scientificPrim, textPrim
    , timeOfDayPrim, utcTimePrim, uuidPrim
    )

type Cookie :: Type -> Type
data Cookie a

data ParseError = ParseError deriving (Eq, Show)

type instance PartOf       Cookie = ByteString
type instance ParseErrorOf Cookie = ParseError

cookieIso :: Prim a -> Iso Cookie a
cookieIso (Prim dec enc nfo) =
    Iso (\bs -> either (const (Left ParseError)) Right (dec (decodeUtf8Lenient bs)))
        (encodeUtf8 . enc)
        nfo

instance Data Cookie Int        where iso = cookieIso intPrim
instance Data Cookie Int16      where iso = cookieIso int16Prim
instance Data Cookie Int32      where iso = cookieIso int32Prim
instance Data Cookie Int64      where iso = cookieIso int64Prim
instance Data Cookie Integer    where iso = cookieIso integerPrim
instance Data Cookie Float      where iso = cookieIso floatPrim
instance Data Cookie Double     where iso = cookieIso doublePrim
instance Data Cookie Scientific where iso = cookieIso scientificPrim
instance Data Cookie Bool       where iso = cookieIso boolPrim
instance Data Cookie Char       where iso = cookieIso charPrim
instance Data Cookie Text       where iso = cookieIso textPrim
instance Data Cookie Day        where iso = cookieIso dayPrim
instance Data Cookie LocalTime  where iso = cookieIso localTimePrim
instance Data Cookie UTCTime    where iso = cookieIso utcTimePrim
instance Data Cookie TimeOfDay  where iso = cookieIso timeOfDayPrim
instance Data Cookie UUID       where iso = cookieIso uuidPrim
instance Data Cookie ByteString where iso = Iso Right id (Info "string" Nothing)
