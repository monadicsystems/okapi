
module Okapi.HTTP.RFC9651.BareItem (
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
import Data.Word (Word8)
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Piece)

data ParseError = ParseError deriving (Eq, Show)

eParse :: Either e a -> Either ParseError a
eParse = either (const (Left ParseError)) Right

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

firstTop :: Word8 -> ByteString -> Maybe Int
firstTop sep bs = go False (0 :: Int) 0
  where
    n = BS.length bs
    go inQ depth i
        | i >= n = Nothing
        | otherwise =
            let w = BS.index bs i
            in if w == 34 then go (not inQ) depth (i + 1)
               else if inQ then go inQ depth (i + 1)
               else if w == 40 then go inQ (depth + 1) (i + 1)
               else if w == 41 then go inQ (max 0 (depth - 1)) (i + 1)
               else if w == sep && depth == 0 then Just i
               else go inQ depth (i + 1)

splitTop :: Word8 -> ByteString -> [ByteString]
splitTop sep bs = case firstTop sep bs of
    Nothing -> [bs]
    Just i  -> BS.take i bs : splitTop sep (BS.drop (i + 1) bs)

parseInnerBare :: ByteString -> Maybe [ByteString]
parseInnerBare m = case BS.uncons (strip m) of
    Just (40, rest) -> case firstTop 41 rest of
        Just i  -> Just (filter (not . BS.null) (map strip (splitTop 32 (BS.take i rest))))
        Nothing -> Nothing
    _ -> Nothing

data BareItem a

type instance Piece BareItem = ByteString
type instance Failure BareItem = ParseError

bareLeaf :: (ByteString -> Either Text a) -> (a -> ByteString) -> Info -> Leaf BareItem a
bareLeaf dec enc nfo = Leaf (eParse . dec) enc nfo

decInteger :: ByteString -> Either Text Integer
decInteger bs = case BS8.readInteger bs of
    Just (n, r) | BS.null r -> Right n
    _ -> Left ("invalid sf-integer: " <> decodeUtf8Lenient bs)

instance HasLeaf BareItem Integer where
    leaf = bareLeaf decInteger (BS8.pack . show) (Info "integer" Nothing)

instance HasLeaf BareItem Int where
    leaf = bareLeaf (fmap fromInteger . decInteger) (BS8.pack . show) (Info "integer" Nothing)

instance HasLeaf BareItem Bool where
    leaf = bareLeaf dec enc (Info "boolean" Nothing)
      where
        enc True  = "?1"
        enc False = "?0"
        dec "?1"  = Right True
        dec "?0"  = Right False
        dec bs    = Left ("invalid sf-boolean: " <> decodeUtf8Lenient bs)

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

instance HasLeaf BareItem Text where
    leaf = bareLeaf parseSfString (\t -> "\"" <> escapeSfString (encodeUtf8 t) <> "\"") (Info "string" Nothing)

instance HasLeaf BareItem ByteString where
    leaf = bareLeaf Right id (Info "string" Nothing)

renderSfDecimal :: Scientific -> ByteString
renderSfDecimal = BS8.pack . formatScientific Fixed (Just 3)

parseSfDecimal :: ByteString -> Either Text Scientific
parseSfDecimal bs = case BS8.readInt bs of
    _ -> case reads (BS8.unpack bs) of
        [(s, "")] -> Right s
        _ -> Left ("invalid sf-decimal: " <> decodeUtf8Lenient bs)

instance HasLeaf BareItem Scientific where
    leaf = bareLeaf parseSfDecimal renderSfDecimal (Info "number" Nothing)

instance HasLeaf BareItem Double where
    leaf = bareLeaf (fmap toRealFloat . parseSfDecimal) (renderSfDecimal . realToFrac) (Info "number" (Just "double"))

instance HasLeaf BareItem Float where
    leaf = bareLeaf (fmap toRealFloat . parseSfDecimal) (renderSfDecimal . realToFrac) (Info "number" (Just "float"))

instance HasLeaf BareItem UTCTime where
    leaf = bareLeaf dec enc (Info "string" (Just "date-time"))
      where
        enc t = BS8.cons '@' (BS8.pack (show (truncate (utcTimeToPOSIXSeconds t) :: Integer)))
        dec bs = case BS8.uncons bs of
            Just ('@', rest) -> case BS8.readInteger rest of
                Just (n, r) | BS.null r -> Right (posixSecondsToUTCTime (fromInteger n))
                _ -> Left ("invalid sf-date: " <> decodeUtf8Lenient bs)
            _ -> Left ("invalid sf-date (expected '@'): " <> decodeUtf8Lenient bs)

renderInner :: Leaf BareItem a -> [a] -> ByteString
renderInner vLeaf xs = "(" <> BS.intercalate " " (map vLeaf.encode xs) <> ")"

parseInnerToList :: Leaf BareItem a -> ByteString -> Either ParseError [a]
parseInnerToList vLeaf v = case parseInnerBare (strip v) of
    Just xs -> traverse vLeaf.decode xs
    Nothing -> Left ParseError
