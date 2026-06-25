{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Protocol.Headers.Structured.BareItem (
    BareItem,
    renderInner,
    parseInnerToList,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Okapi.Codec (ParseErrorOf, PartOf)
import Okapi.Data (Data (..), Info (..), Iso (..))
import Okapi.Protocol.Headers.Structured.Lexer (ParseError (..), eParse, parseInnerBare, strip)

data BareItem a

type instance PartOf BareItem = ByteString
type instance ParseErrorOf BareItem = ParseError

bareIso :: (ByteString -> Either Text a) -> (a -> ByteString) -> Info -> Iso BareItem a
bareIso dec enc nfo = Iso (eParse . dec) enc nfo

decInteger :: ByteString -> Either Text Integer
decInteger bs = case BS8.readInteger bs of
    Just (n, r) | BS.null r -> Right n
    _ -> Left ("invalid sf-integer: " <> decodeUtf8Lenient bs)

instance Data BareItem Integer where
    iso = bareIso decInteger (BS8.pack . show) (Info "integer" Nothing)

instance Data BareItem Int where
    iso = bareIso (fmap fromInteger . decInteger) (BS8.pack . show) (Info "integer" Nothing)

instance Data BareItem Bool where
    iso = bareIso dec enc (Info "boolean" Nothing)
      where
        enc True = "?1"
        enc False = "?0"
        dec "?1" = Right True
        dec "?0" = Right False
        dec bs = Left ("invalid sf-boolean: " <> decodeUtf8Lenient bs)

escapeSfString :: ByteString -> ByteString
escapeSfString = BS.concatMap esc
  where
    esc 34 = "\\\""
    esc 92 = "\\\\"
    esc w  = BS.singleton w

parseSfString :: ByteString -> Either Text Text
parseSfString bs = case BS.uncons bs of
    Just (34, rest) -> go rest mempty
    _ -> Left ("invalid sf-string (no opening quote): " <> decodeUtf8Lenient bs)
  where
    go s acc = case BS.uncons s of
        Nothing -> Left "invalid sf-string (unterminated)"
        Just (34, r)
            | BS.null r -> Right (decodeUtf8Lenient acc)
            | otherwise -> Left "invalid sf-string (trailing bytes after close)"
        Just (92, r) -> case BS.uncons r of
            Just (c, r') | c == 34 || c == 92 -> go r' (acc <> BS.singleton c)
            _ -> Left "invalid sf-string (bad escape)"
        Just (c, r) -> go r (acc <> BS.singleton c)

instance Data BareItem Text where
    iso = bareIso parseSfString (\t -> "\"" <> escapeSfString (encodeUtf8 t) <> "\"") (Info "string" Nothing)

instance Data BareItem ByteString where
    iso = bareIso Right id (Info "string" Nothing)

renderSfDecimal :: Scientific -> ByteString
renderSfDecimal = BS8.pack . formatScientific Fixed (Just 3)

parseSfDecimal :: ByteString -> Either Text Scientific
parseSfDecimal bs = case BS8.readInt bs of
    _ -> case reads (BS8.unpack bs) of
        [(s, "")] -> Right s
        _ -> Left ("invalid sf-decimal: " <> decodeUtf8Lenient bs)

instance Data BareItem Scientific where
    iso = bareIso parseSfDecimal renderSfDecimal (Info "number" Nothing)

instance Data BareItem Double where
    iso = bareIso (fmap toRealFloat . parseSfDecimal) (renderSfDecimal . realToFrac) (Info "number" (Just "double"))

instance Data BareItem Float where
    iso = bareIso (fmap toRealFloat . parseSfDecimal) (renderSfDecimal . realToFrac) (Info "number" (Just "float"))

instance Data BareItem UTCTime where
    iso = bareIso dec enc (Info "string" (Just "date-time"))
      where
        enc t = BS8.cons '@' (BS8.pack (show (truncate (utcTimeToPOSIXSeconds t) :: Integer)))
        dec bs = case BS8.uncons bs of
            Just ('@', rest) -> case BS8.readInteger rest of
                Just (n, r) | BS.null r -> Right (posixSecondsToUTCTime (fromInteger n))
                _ -> Left ("invalid sf-date: " <> decodeUtf8Lenient bs)
            _ -> Left ("invalid sf-date (expected '@'): " <> decodeUtf8Lenient bs)

renderInner :: Iso BareItem a -> [a] -> ByteString
renderInner vIso xs = "(" <> BS.intercalate " " (map vIso.encode xs) <> ")"

parseInnerToList :: Iso BareItem a -> ByteString -> Either ParseError [a]
parseInnerToList vIso v = case parseInnerBare (strip v) of
    Just xs -> traverse vIso.decode xs
    Nothing -> Left ParseError
