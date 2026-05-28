{-# LANGUAGE OverloadedStrings #-}

module Okapi.Data (
    ToPathData (..),
    FromPathData (..),
    IsoPathData,
    ToQueryData (..),
    FromQueryData (..),
    IsoQueryData,
    ToHeaderData (..),
    FromHeaderData (..),
    IsoHeaderData,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (Day, DiffTime, LocalTime (..), TimeOfDay, TimeZone, UTCTime, ZonedTime (..), timeZoneOffsetString)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Type classes

class ToPathData a where
    toUrlPiece :: a -> Text

class FromPathData a where
    parseUrlPiece :: Text -> Either Text a

type IsoPathData a = (ToPathData a, FromPathData a, Typeable a)

class ToQueryData a where
    toQueryParam :: a -> Text

class FromQueryData a where
    parseQueryParam :: Text -> Either Text a

type IsoQueryData a = (ToQueryData a, FromQueryData a, Typeable a)

class ToHeaderData a where
    toHeader :: a -> ByteString

class FromHeaderData a where
    parseHeader :: ByteString -> Either Text a

type IsoHeaderData a = (ToHeaderData a, FromHeaderData a, Typeable a)

-- ---------------------------------------------------------------------------
-- Helpers

parseHeaderViaText :: FromQueryData a => ByteString -> Either Text a
parseHeaderViaText bs = first (T.pack . show) (decodeUtf8' bs) >>= parseQueryParam

toHeaderViaText :: ToQueryData a => a -> ByteString
toHeaderViaText = encodeUtf8 . toQueryParam

readEither :: Read a => Text -> Text -> Either Text a
readEither typeName t = case readMaybe (T.unpack t) of
    Just n  -> Right n
    Nothing -> Left ("Invalid " <> typeName <> ": " <> t)

-- ---------------------------------------------------------------------------
-- Bool

instance ToPathData Bool where
    toUrlPiece True  = "true"
    toUrlPiece False = "false"

instance FromPathData Bool where
    parseUrlPiece t = case T.toLower t of
        "true"  -> Right True
        "false" -> Right False
        "1"     -> Right True
        "0"     -> Right False
        _       -> Left ("Invalid Bool: " <> t)

instance ToQueryData  Bool where toQueryParam    = toUrlPiece
instance FromQueryData Bool where parseQueryParam = parseUrlPiece
instance ToHeaderData  Bool where toHeader        = toHeaderViaText
instance FromHeaderData Bool where parseHeader    = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- Char

instance ToPathData Char where
    toUrlPiece = T.singleton

instance FromPathData Char where
    parseUrlPiece t
        | T.length t == 1 = Right (T.head t)
        | otherwise       = Left ("Expected single character: " <> t)

instance ToQueryData  Char where toQueryParam    = toUrlPiece
instance FromQueryData Char where parseQueryParam = parseUrlPiece

-- ---------------------------------------------------------------------------
-- Int

instance ToPathData   Int where toUrlPiece      = T.pack . show
instance FromPathData  Int where parseUrlPiece   = readEither "Int"
instance ToQueryData   Int where toQueryParam    = toUrlPiece
instance FromQueryData Int where parseQueryParam = parseUrlPiece
instance ToHeaderData  Int where toHeader        = toHeaderViaText
instance FromHeaderData Int where parseHeader    = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- Int16, Int32, Int64

instance ToPathData    Int16 where toUrlPiece      = T.pack . show
instance FromPathData  Int16 where parseUrlPiece   = readEither "Int16"
instance ToQueryData   Int16 where toQueryParam    = toUrlPiece
instance FromQueryData Int16 where parseQueryParam = parseUrlPiece
instance ToHeaderData  Int16 where toHeader        = toHeaderViaText
instance FromHeaderData Int16 where parseHeader    = parseHeaderViaText

instance ToPathData    Int32 where toUrlPiece      = T.pack . show
instance FromPathData  Int32 where parseUrlPiece   = readEither "Int32"
instance ToQueryData   Int32 where toQueryParam    = toUrlPiece
instance FromQueryData Int32 where parseQueryParam = parseUrlPiece
instance ToHeaderData  Int32 where toHeader        = toHeaderViaText
instance FromHeaderData Int32 where parseHeader    = parseHeaderViaText

instance ToPathData    Int64 where toUrlPiece      = T.pack . show
instance FromPathData  Int64 where parseUrlPiece   = readEither "Int64"
instance ToQueryData   Int64 where toQueryParam    = toUrlPiece
instance FromQueryData Int64 where parseQueryParam = parseUrlPiece
instance ToHeaderData  Int64 where toHeader        = toHeaderViaText
instance FromHeaderData Int64 where parseHeader    = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- Float, Double

instance ToPathData   Float where toUrlPiece      = T.pack . show
instance FromPathData  Float where parseUrlPiece   = readEither "Float"
instance ToQueryData   Float where toQueryParam    = toUrlPiece
instance FromQueryData Float where parseQueryParam = parseUrlPiece
instance ToHeaderData  Float where toHeader        = toHeaderViaText
instance FromHeaderData Float where parseHeader    = parseHeaderViaText

instance ToPathData    Double where toUrlPiece      = T.pack . show
instance FromPathData  Double where parseUrlPiece   = readEither "Double"
instance ToQueryData   Double where toQueryParam    = toUrlPiece
instance FromQueryData Double where parseQueryParam = parseUrlPiece
instance ToHeaderData  Double where toHeader        = toHeaderViaText
instance FromHeaderData Double where parseHeader    = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- Scientific (query + header)

instance ToQueryData   Scientific where toQueryParam    = T.pack . show
instance FromQueryData Scientific where parseQueryParam = readEither "Scientific"
instance ToHeaderData  Scientific where toHeader        = toHeaderViaText
instance FromHeaderData Scientific where parseHeader    = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- Text

instance ToPathData   Text where toUrlPiece      = id
instance FromPathData  Text where parseUrlPiece   = Right
instance ToQueryData   Text where toQueryParam    = id
instance FromQueryData Text where parseQueryParam = Right
instance ToHeaderData  Text where toHeader        = encodeUtf8
instance FromHeaderData Text where parseHeader    = first (T.pack . show) . decodeUtf8'

-- ---------------------------------------------------------------------------
-- ByteString (header only)

instance ToHeaderData   ByteString where toHeader    = id
instance FromHeaderData ByteString where parseHeader = Right

-- ---------------------------------------------------------------------------
-- Day (query + header)

instance ToQueryData Day where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

instance FromQueryData Day where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
        Just d  -> Right d
        Nothing -> Left ("Invalid Day: " <> t)

instance ToHeaderData  Day where toHeader    = toHeaderViaText
instance FromHeaderData Day where parseHeader = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- LocalTime (query + header)

instance ToQueryData LocalTime where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance FromQueryData LocalTime where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack t) of
        Just dt -> Right dt
        Nothing -> Left ("Invalid LocalTime: " <> t)

instance ToHeaderData  LocalTime where toHeader    = toHeaderViaText
instance FromHeaderData LocalTime where parseHeader = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- UTCTime (query + header)

instance ToQueryData UTCTime where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

instance FromQueryData UTCTime where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack t) of
        Just dt -> Right dt
        Nothing -> Left ("Invalid UTCTime: " <> t)

instance ToHeaderData  UTCTime where toHeader    = toHeaderViaText
instance FromHeaderData UTCTime where parseHeader = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- TimeOfDay (query + header)

instance ToQueryData TimeOfDay where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%H:%M:%S"

instance FromQueryData TimeOfDay where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%H:%M:%S" (T.unpack t) of
        Just tod -> Right tod
        Nothing  -> Left ("Invalid TimeOfDay: " <> t)

instance ToHeaderData  TimeOfDay where toHeader    = toHeaderViaText
instance FromHeaderData TimeOfDay where parseHeader = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- (TimeOfDay, TimeZone) (header only)

instance ToHeaderData (TimeOfDay, TimeZone) where
    toHeader (tod, tz) = encodeUtf8 . T.pack $
        formatTime defaultTimeLocale "%T" tod <> timeZoneOffsetString tz

instance FromHeaderData (TimeOfDay, TimeZone) where
    parseHeader bs = do
        t <- first (T.pack . show) (decodeUtf8' bs)
        case parseTimeM True defaultTimeLocale "%T%z" (T.unpack t) of
            Just zt -> Right (localTimeOfDay (zonedTimeToLocalTime zt), zonedTimeZone zt)
            Nothing -> Left ("Invalid (TimeOfDay, TimeZone): " <> t)

-- ---------------------------------------------------------------------------
-- DiffTime (header only)

instance ToHeaderData DiffTime where
    toHeader = encodeUtf8 . T.pack . show . (realToFrac :: DiffTime -> Double)

instance FromHeaderData DiffTime where
    parseHeader bs = do
        t <- first (T.pack . show) (decodeUtf8' bs)
        case (readMaybe (T.unpack t) :: Maybe Double) of
            Just d  -> Right (realToFrac d)
            Nothing -> Left ("Invalid DiffTime: " <> t)

-- ---------------------------------------------------------------------------
-- UUID (path + query + header)

instance ToPathData UUID where
    toUrlPiece = UUID.toText

instance FromPathData UUID where
    parseUrlPiece t = case UUID.fromText t of
        Just u  -> Right u
        Nothing -> Left ("Invalid UUID: " <> t)

instance ToQueryData   UUID where toQueryParam    = UUID.toText
instance FromQueryData UUID where
    parseQueryParam t = case UUID.fromText t of
        Just u  -> Right u
        Nothing -> Left ("Invalid UUID: " <> t)

instance ToHeaderData  UUID where toHeader        = toHeaderViaText
instance FromHeaderData UUID where parseHeader    = parseHeaderViaText

-- ---------------------------------------------------------------------------
-- Maybe a (query + header)

instance ToQueryData a => ToQueryData (Maybe a) where
    toQueryParam Nothing  = ""
    toQueryParam (Just x) = toQueryParam x

instance FromQueryData a => FromQueryData (Maybe a) where
    parseQueryParam t
        | T.null t  = Right Nothing
        | otherwise = fmap Just (parseQueryParam t)

instance ToHeaderData a => ToHeaderData (Maybe a) where
    toHeader Nothing  = ""
    toHeader (Just x) = toHeader x

instance FromHeaderData a => FromHeaderData (Maybe a) where
    parseHeader bs
        | BS.null bs = Right Nothing
        | otherwise  = fmap Just (parseHeader bs)

-- ---------------------------------------------------------------------------
-- [a] and Vector a (query only)

instance ToQueryData a => ToQueryData [a] where
    toQueryParam = T.intercalate "," . map toQueryParam

instance FromQueryData a => FromQueryData [a] where
    parseQueryParam t
        | T.null t  = Right []
        | otherwise = mapM parseQueryParam (T.splitOn "," t)

instance ToQueryData a => ToQueryData (Vector a) where
    toQueryParam = toQueryParam . V.toList

instance FromQueryData a => FromQueryData (Vector a) where
    parseQueryParam t = fmap V.fromList (parseQueryParam t)
