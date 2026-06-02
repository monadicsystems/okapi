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
    ToCookieData (..),
    FromCookieData (..),
    IsoCookieData,
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


parseHeaderViaText :: FromQueryData a => ByteString -> Either Text a
parseHeaderViaText bs = first (T.pack . show) (decodeUtf8' bs) >>= parseQueryParam

toHeaderViaText :: ToQueryData a => a -> ByteString
toHeaderViaText = encodeUtf8 . toQueryParam

readEither :: Read a => Text -> Text -> Either Text a
readEither typeName t = case readMaybe (T.unpack t) of
    Just n  -> Right n
    Nothing -> Left ("Invalid " <> typeName <> ": " <> t)


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


instance ToPathData Char where
    toUrlPiece = T.singleton

instance FromPathData Char where
    parseUrlPiece t
        | T.length t == 1 = Right (T.head t)
        | otherwise       = Left ("Expected single character: " <> t)

instance ToQueryData  Char where toQueryParam    = toUrlPiece
instance FromQueryData Char where parseQueryParam = parseUrlPiece


instance ToPathData   Int where toUrlPiece      = T.pack . show
instance FromPathData  Int where parseUrlPiece   = readEither "Int"
instance ToQueryData   Int where toQueryParam    = toUrlPiece
instance FromQueryData Int where parseQueryParam = parseUrlPiece
instance ToHeaderData  Int where toHeader        = toHeaderViaText
instance FromHeaderData Int where parseHeader    = parseHeaderViaText


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


instance ToQueryData   Scientific where toQueryParam    = T.pack . show
instance FromQueryData Scientific where parseQueryParam = readEither "Scientific"
instance ToHeaderData  Scientific where toHeader        = toHeaderViaText
instance FromHeaderData Scientific where parseHeader    = parseHeaderViaText


instance ToPathData   Text where toUrlPiece      = id
instance FromPathData  Text where parseUrlPiece   = Right
instance ToQueryData   Text where toQueryParam    = id
instance FromQueryData Text where parseQueryParam = Right
instance ToHeaderData  Text where toHeader        = encodeUtf8
instance FromHeaderData Text where parseHeader    = first (T.pack . show) . decodeUtf8'


instance ToHeaderData   ByteString where toHeader    = id
instance FromHeaderData ByteString where parseHeader = Right


instance ToQueryData Day where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

instance FromQueryData Day where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
        Just d  -> Right d
        Nothing -> Left ("Invalid Day: " <> t)

instance ToHeaderData  Day where toHeader    = toHeaderViaText
instance FromHeaderData Day where parseHeader = parseHeaderViaText


instance ToQueryData LocalTime where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance FromQueryData LocalTime where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack t) of
        Just dt -> Right dt
        Nothing -> Left ("Invalid LocalTime: " <> t)

instance ToHeaderData  LocalTime where toHeader    = toHeaderViaText
instance FromHeaderData LocalTime where parseHeader = parseHeaderViaText


instance ToQueryData UTCTime where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

instance FromQueryData UTCTime where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack t) of
        Just dt -> Right dt
        Nothing -> Left ("Invalid UTCTime: " <> t)

instance ToHeaderData  UTCTime where toHeader    = toHeaderViaText
instance FromHeaderData UTCTime where parseHeader = parseHeaderViaText


instance ToQueryData TimeOfDay where
    toQueryParam = T.pack . formatTime defaultTimeLocale "%H:%M:%S"

instance FromQueryData TimeOfDay where
    parseQueryParam t = case parseTimeM True defaultTimeLocale "%H:%M:%S" (T.unpack t) of
        Just tod -> Right tod
        Nothing  -> Left ("Invalid TimeOfDay: " <> t)

instance ToHeaderData  TimeOfDay where toHeader    = toHeaderViaText
instance FromHeaderData TimeOfDay where parseHeader = parseHeaderViaText


instance ToHeaderData (TimeOfDay, TimeZone) where
    toHeader (tod, tz) = encodeUtf8 . T.pack $
        formatTime defaultTimeLocale "%T" tod <> timeZoneOffsetString tz

instance FromHeaderData (TimeOfDay, TimeZone) where
    parseHeader bs = do
        t <- first (T.pack . show) (decodeUtf8' bs)
        case parseTimeM True defaultTimeLocale "%T%z" (T.unpack t) of
            Just zt -> Right (localTimeOfDay (zonedTimeToLocalTime zt), zonedTimeZone zt)
            Nothing -> Left ("Invalid (TimeOfDay, TimeZone): " <> t)


instance ToHeaderData DiffTime where
    toHeader = encodeUtf8 . T.pack . show . (realToFrac :: DiffTime -> Double)

instance FromHeaderData DiffTime where
    parseHeader bs = do
        t <- first (T.pack . show) (decodeUtf8' bs)
        case (readMaybe (T.unpack t) :: Maybe Double) of
            Just d  -> Right (realToFrac d)
            Nothing -> Left ("Invalid DiffTime: " <> t)


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


class ToCookieData a where
    toCookieValue :: a -> ByteString

class FromCookieData a where
    parseCookieValue :: ByteString -> Either Text a

type IsoCookieData a = (ToCookieData a, FromCookieData a, Typeable a)

parseCookieViaText :: FromQueryData a => ByteString -> Either Text a
parseCookieViaText = parseHeaderViaText


instance ToCookieData   Bool where toCookieValue   = toHeaderViaText
instance FromCookieData Bool where parseCookieValue = parseCookieViaText

instance ToCookieData   Char where toCookieValue   = encodeUtf8 . toUrlPiece
instance FromCookieData Char where parseCookieValue bs = first (T.pack . show) (decodeUtf8' bs) >>= parseUrlPiece

instance ToCookieData   Int where toCookieValue   = toHeaderViaText
instance FromCookieData Int where parseCookieValue = parseCookieViaText

instance ToCookieData   Int16 where toCookieValue   = toHeaderViaText
instance FromCookieData Int16 where parseCookieValue = parseCookieViaText

instance ToCookieData   Int32 where toCookieValue   = toHeaderViaText
instance FromCookieData Int32 where parseCookieValue = parseCookieViaText

instance ToCookieData   Int64 where toCookieValue   = toHeaderViaText
instance FromCookieData Int64 where parseCookieValue = parseCookieViaText

instance ToCookieData   Float where toCookieValue   = toHeaderViaText
instance FromCookieData Float where parseCookieValue = parseCookieViaText

instance ToCookieData   Double where toCookieValue   = toHeaderViaText
instance FromCookieData Double where parseCookieValue = parseCookieViaText

instance ToCookieData   Scientific where toCookieValue   = toHeaderViaText
instance FromCookieData Scientific where parseCookieValue = parseCookieViaText

instance ToCookieData   Text where toCookieValue   = encodeUtf8
instance FromCookieData Text where parseCookieValue = first (T.pack . show) . decodeUtf8'

instance ToCookieData   ByteString where toCookieValue   = id
instance FromCookieData ByteString where parseCookieValue = Right

instance ToCookieData   Day where toCookieValue   = toHeaderViaText
instance FromCookieData Day where parseCookieValue = parseCookieViaText

instance ToCookieData   LocalTime where toCookieValue   = toHeaderViaText
instance FromCookieData LocalTime where parseCookieValue = parseCookieViaText

instance ToCookieData   UTCTime where toCookieValue   = toHeaderViaText
instance FromCookieData UTCTime where parseCookieValue = parseCookieViaText

instance ToCookieData   TimeOfDay where toCookieValue   = toHeaderViaText
instance FromCookieData TimeOfDay where parseCookieValue = parseCookieViaText

instance ToCookieData   UUID where toCookieValue   = toHeaderViaText
instance FromCookieData UUID where parseCookieValue = parseCookieViaText

instance ToCookieData a => ToCookieData (Maybe a) where
    toCookieValue Nothing  = ""
    toCookieValue (Just x) = toCookieValue x

instance FromCookieData a => FromCookieData (Maybe a) where
    parseCookieValue bs
        | BS.null bs = Right Nothing
        | otherwise  = fmap Just (parseCookieValue bs)
