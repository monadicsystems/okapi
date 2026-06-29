{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Cookies (
    Cookie,
    ParseError (..),
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (Day)
import Data.UUID (UUID)
import Okapi.Codec (ParseErrorOf, PartOf)
import Okapi.Data (Data (..), Info (..), Iso (..))
import Web.HttpApiData (parseHeader, toHeader)

type Cookie :: Type -> Type
data Cookie a

data ParseError = ParseError deriving (Eq, Show)

type instance PartOf Cookie = ByteString
type instance ParseErrorOf Cookie = ParseError

instance Data Cookie Int where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance Data Cookie Int16 where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance Data Cookie Int32 where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance Data Cookie Int64 where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int64"))
instance Data Cookie Integer where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance Data Cookie Bool where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "boolean" Nothing)
instance Data Cookie Text where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "string" Nothing)
instance Data Cookie UUID where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "uuid"))
instance Data Cookie Day where
    iso = Iso (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date"))

instance Data Cookie ByteString where
    iso = Iso Right id (Info "string" Nothing)
