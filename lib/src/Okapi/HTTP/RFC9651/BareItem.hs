
module Okapi.HTTP.RFC9651.BareItem (
    BareItem,
    hasNonCanonicalInteger,
    DisplayString (..),
    displayString,
    Token,
    mkToken,
    unToken,
    token,
    ByteSequence (..),
    byteSequence,
    renderInner,
    parseInnerToList,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', decodeUtf8Lenient, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Numeric (showHex)
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Piece)
import Okapi.HTTP.RFC9651.Scan (strip, firstTop, splitTop)

-- $setup
-- >>> import Okapi.Tree (Leaf (..), HasLeaf (..), leafPrintParse, leafParsePrint, leafParsePrintOr, leafPrintParsePrint, leafParsePrintParse, integer, bool, text, scientific, double, float, utcTime)
-- >>> import Data.ByteString.Char8 qualified as BS8
-- >>> import Data.Text (Text)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.ByteString qualified as BS
-- >>> import Data.Scientific (Scientific)
-- >>> import Data.Time.Clock (UTCTime)
-- >>> import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Test.QuickCheck (Gen, (==>), arbitrary, discard, forAll, frequency)
-- >>> let mixedInteger = frequency [(1, arbitrary), (3, BS8.pack . show <$> (arbitrary :: Gen Integer))] :: Gen ByteString
-- >>> let mixedDisplayString = frequency [(1, arbitrary), (3, (encode (displayString :: Leaf BareItem DisplayString) . DisplayString) <$> (arbitrary :: Gen Text))] :: Gen ByteString

data ParseError = ParseError deriving (Eq, Show)

eParse :: Either e a -> Either ParseError a
eParse = either (const (Left ParseError)) Right

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

-- | RFC 9651 §3.3.1: @sf-integer = ["-"] 1*15DIGIT@ — no leading @+@,
--   unlike 'BS8.readInteger', which accepts one (e.g. @"+0"@) and would
--   otherwise decode successfully but re-encode as plain @"0"@, breaking
--   the round trip.
decInteger :: ByteString -> Either Text Integer
decInteger bs = case BS.uncons bs of
    Just (0x2b, _) -> invalid
    _ -> case BS8.readInteger bs of
        Just (n, r) | BS.null r -> Right n
        _                       -> invalid
  where
    invalid = Left ("invalid sf-integer: " <> decodeUtf8Lenient bs)

-- | RFC 9651's own parsing algorithm (§4.2.4) accepts leading zeros
--   (@\"007\"@) and negative zero (@\"-0\"@) without rejecting them — its
--   serialization algorithm (§4.1.4) never produces either, so they're
--   syntactically legal but non-canonical: decoding them succeeds, but
--   re-encoding the result never reproduces the original bytes (@\"007\"@
--   decodes to 7, which re-encodes as @\"7\"@; @\"-0\"@ decodes to 0, which
--   re-encodes as @\"0\"@). True everywhere an @sf-integer@ can appear —
--   used to exclude these RFC-permitted-but-non-canonical shapes from
--   @parsePrint@\/@leafParsePrint@ properties, here and in every module
--   that wraps an integer leaf (@Item@, @Parameters@, @Dictionary@),
--   rather than making 'decInteger' reject valid RFC 9651 syntax just to
--   dodge the test.
hasNonCanonicalInteger :: ByteString -> Bool
hasNonCanonicalInteger bs = any badAt (BS.tails bs)
  where
    badAt t = case BS8.uncons t of
        Just ('-', t') -> case BS8.uncons t' of
            Just ('0', _) -> True
            _              -> False
        Just ('0', rest) -> not (BS8.null rest) && isDigitB (BS8.head rest)
        _ -> False
    isDigitB c = c >= '0' && c <= '9'

-- |
-- >>> decode (integer :: Leaf BareItem Integer) "42"
-- Right 42
-- >>> encode (integer :: Leaf BareItem Integer) 42
-- "42"
--
-- prop> leafPrintParse (decode (integer :: Leaf BareItem Integer)) (encode (integer :: Leaf BareItem Integer)) x
-- prop> forAll mixedInteger (\bs -> not (hasNonCanonicalInteger bs) ==> leafParsePrintOr discard (decode (integer :: Leaf BareItem Integer)) (encode (integer :: Leaf BareItem Integer)) bs)
instance HasLeaf BareItem Integer where
    leaf = bareLeaf decInteger (BS8.pack . show) (Info "integer" Nothing)

instance HasLeaf BareItem Int where
    leaf = bareLeaf (fmap fromInteger . decInteger) (BS8.pack . show) (Info "integer" Nothing)

-- |
-- >>> decode (bool :: Leaf BareItem Bool) "?1"
-- Right True
-- >>> encode (bool :: Leaf BareItem Bool) True
-- "?1"
--
-- prop> leafPrintParse (decode (bool :: Leaf BareItem Bool)) (encode (bool :: Leaf BareItem Bool)) x
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

-- | Encoded as a quoted @sf-string@, with @"@ and @\\@ escaped.
--
-- >>> decode (text :: Leaf BareItem Text) "\"hello\""
-- Right "hello"
-- >>> encode (text :: Leaf BareItem Text) "hello"
-- "\"hello\""
-- >>> encode (text :: Leaf BareItem Text) "a\"b"
-- "\"a\\\"b\""
-- >>> decode (text :: Leaf BareItem Text) (encode (text :: Leaf BareItem Text) "a\"b")
-- Right "a\"b"
--
-- prop> leafPrintParse (decode (text :: Leaf BareItem Text)) (encode (text :: Leaf BareItem Text)) x
instance HasLeaf BareItem Text where
    leaf = bareLeaf parseSfString (\t -> "\"" <> escapeSfString (encodeUtf8 t) <> "\"") (Info "string" Nothing)

-- | An RFC 9651 §3.3.8 Display String — conceptually the same kind of
--   text as @sf-string@\/'Text' (§3.3.3), but encoded on the wire with a
--   leading @%@ marker and byte-wise percent-encoding for non-ASCII and
--   control bytes, rather than 'Text'\'s backslash-escaped, ASCII-only
--   form. A distinct newtype, not a bare 'Text' alias — 'HasLeaf' can only
--   have one instance per type, and @sf-string@ already claims 'Text'.
newtype DisplayString = DisplayString Text
    deriving (Eq, Ord, Show)

displayStringEscapes :: Word8 -> Bool
displayStringEscapes w = w == 0x25 || w == 0x22 || w < 0x20 || w >= 0x7f

toHex2 :: Word8 -> ByteString
toHex2 w = BS8.pack (case showHex w "" of [c] -> ['0', c]; s -> s)

fromHex2 :: Word8 -> Word8 -> Maybe Word8
fromHex2 a b = (\x y -> x * 16 + y) <$> digit a <*> digit b
  where
    digit w
        | w >= 0x30 && w <= 0x39 = Just (w - 0x30)
        | w >= 0x61 && w <= 0x66 = Just (w - 0x61 + 10)
        | otherwise              = Nothing

-- | RFC 9651 §4.1.11: percent-encode @%@, @"@, and any byte outside
--   %x20-7E; everything else — including a literal @\\@, unlike 'Text' —
--   passes through unescaped.
renderDisplayString :: Text -> ByteString
renderDisplayString t = "%\"" <> BS.concatMap enc (encodeUtf8 t) <> "\""
  where
    enc w
        | displayStringEscapes w = "%" <> toHex2 w
        | otherwise               = BS.singleton w

-- | RFC 9651 §4.2.10.
parseDisplayString :: ByteString -> Either Text Text
parseDisplayString bs = case BS.uncons bs of
    Just (0x25, rest) -> case BS.uncons rest of
        Just (0x22, rest') -> go rest' mempty
        _                  -> invalid
    _ -> invalid
  where
    invalid = Left ("invalid sf-displaystring (expected '%\"'): " <> decodeUtf8Lenient bs)
    go s acc = case BS.uncons s of
        Nothing -> Left "invalid sf-displaystring (unterminated)"
        Just (0x22, r)
            | BS.null r -> either (const (Left "invalid sf-displaystring (bad utf-8)")) Right (decodeUtf8' acc)
            | otherwise -> Left "invalid sf-displaystring (trailing bytes after close)"
        Just (0x25, r) -> case BS.uncons r of
            Just (h1, r1) -> case BS.uncons r1 of
                Just (h2, r2) -> case fromHex2 h1 h2 of
                    Just byte -> go r2 (acc <> BS.singleton byte)
                    Nothing   -> Left "invalid sf-displaystring (bad percent-encoding)"
                Nothing -> Left "invalid sf-displaystring (truncated percent-encoding)"
            Nothing -> Left "invalid sf-displaystring (truncated percent-encoding)"
        Just (w, r)
            | w >= 0x20 && w < 0x7f -> go r (acc <> BS.singleton w)
            | otherwise             -> Left "invalid sf-displaystring (unescaped control byte)"

-- | Encoded per RFC 9651 §3.3.8\/§4.1.11\/§4.2.10 — a leading @%@ then a
--   quoted, percent-encoded UTF-8 string. The RFC's own worked example
--   (§3.3.8):
--
-- >>> decode (displayString :: Leaf BareItem DisplayString) "%\"This is intended for display to %c3%bcsers.\""
-- Right (DisplayString "This is intended for display to \252sers.")
-- >>> encode (displayString :: Leaf BareItem DisplayString) (DisplayString "This is intended for display to \252sers.")
-- "%\"This is intended for display to %c3%bcsers.\""
--
-- A literal backslash passes through unescaped, unlike 'Text'\'s
-- @sf-string@ (contrast with the @a\\\"b@ example above):
--
-- >>> encode (displayString :: Leaf BareItem DisplayString) (DisplayString "a\\b")
-- "%\"a\\b\""
-- >>> decode (displayString :: Leaf BareItem DisplayString) "%\"a\\b\""
-- Right (DisplayString "a\\b")
--
-- prop> \t -> leafPrintParse (decode (displayString :: Leaf BareItem DisplayString)) (encode (displayString :: Leaf BareItem DisplayString)) (DisplayString t)
-- prop> forAll mixedDisplayString (\bs -> leafParsePrintOr discard (decode (displayString :: Leaf BareItem DisplayString)) (encode (displayString :: Leaf BareItem DisplayString)) bs)
instance HasLeaf BareItem DisplayString where
    leaf = bareLeaf (fmap DisplayString . parseDisplayString) (\(DisplayString t) -> renderDisplayString t) (Info "string" (Just "display"))

displayString :: (HasLeaf t DisplayString) => Leaf t DisplayString
displayString = leaf

isAsciiAlphaW :: Word8 -> Bool
isAsciiAlphaW w = (w >= 0x41 && w <= 0x5a) || (w >= 0x61 && w <= 0x7a)

isAsciiDigitW :: Word8 -> Bool
isAsciiDigitW w = w >= 0x30 && w <= 0x39

isTokenStart :: Word8 -> Bool
isTokenStart w = isAsciiAlphaW w || w == 0x2a -- '*'

-- | RFC 7230 @tchar@, plus @:@ and @\/@ per RFC 9651 §3.3.4.
isTokenTail :: Word8 -> Bool
isTokenTail w = w == 0x3a || w == 0x2f || isTChar w
  where
    isTChar c = BS.elem c "!#$%&'*+-.^_`|~" || isAsciiDigitW c || isAsciiAlphaW c

-- | An RFC 9651 §3.3.4 Token — a bare, unquoted identifier-like value
--   (e.g. @image\/png@), never escaped on the wire. Not every 'ByteString'
--   is a valid token (must start with a letter or @*@, and be non-empty),
--   so construction goes through 'mkToken' rather than a bare constructor.
newtype Token = Token ByteString
    deriving (Eq, Ord, Show)

unToken :: Token -> ByteString
unToken (Token bs) = bs

-- | 'Nothing' if @bs@ isn't a valid @sf-token@.
--
-- >>> mkToken "image/png"
-- Just (Token "image/png")
-- >>> mkToken "*foo"
-- Just (Token "*foo")
-- >>> mkToken "1abc"
-- Nothing
-- >>> mkToken ""
-- Nothing
mkToken :: ByteString -> Maybe Token
mkToken bs = case BS.uncons bs of
    Just (c, rest) | isTokenStart c && BS.all isTokenTail rest -> Just (Token bs)
    _ -> Nothing

-- | Encoded as the bare, unquoted bytes — RFC 9651 §3.3.4.
--
-- >>> decode (token :: Leaf BareItem Token) "image/png"
-- Right (Token "image/png")
-- >>> encode (token :: Leaf BareItem Token) <$> mkToken "image/png"
-- Just "image/png"
-- >>> decode (token :: Leaf BareItem Token) "1abc"
-- Left ParseError
--
-- prop> \bs -> maybe True (leafPrintParse (decode (token :: Leaf BareItem Token)) (encode (token :: Leaf BareItem Token))) (mkToken ("a" <> BS.filter (\w -> (w >= 48 && w <= 57) || (w >= 65 && w <= 90) || (w >= 97 && w <= 122)) bs))
instance HasLeaf BareItem Token where
    leaf = bareLeaf dec unToken (Info "token" Nothing)
      where
        dec bs = maybe (Left ("invalid sf-token: " <> decodeUtf8Lenient bs)) Right (mkToken bs)

token :: (HasLeaf t Token) => Leaf t Token
token = leaf

-- | An RFC 9651 §3.3.5 Byte Sequence — arbitrary bytes, base64-encoded
--   (RFC 4648 §4, standard alphabet, required padding) and colon-delimited
--   on the wire, e.g. @:cGxlYXN1cmUu:@. Every 'ByteString' is a valid
--   value (unlike 'Token'), so the constructor is exported directly.
newtype ByteSequence = ByteSequence ByteString
    deriving (Eq, Ord, Show)

renderByteSequence :: ByteString -> ByteString
renderByteSequence bs = ":" <> B64.encode bs <> ":"

parseByteSequence :: ByteString -> Either Text ByteString
parseByteSequence bs = case BS.uncons bs of
    Just (0x3a, rest) -> case BS.unsnoc rest of
        Just (body, 0x3a) ->
            either (const (Left invalid)) Right (B64.decode body)
        _ -> Left invalid
    _ -> Left invalid
  where
    invalid = "invalid sf-binary: " <> decodeUtf8Lenient bs

-- | Encoded per RFC 9651 §3.3.5\/§4.1.8\/§4.2.7 — standard base64
--   (RFC 4648 §4), colon-delimited.
--
-- >>> decode (byteSequence :: Leaf BareItem ByteSequence) ":cGxlYXN1cmUu:"
-- Right (ByteSequence "pleasure.")
-- >>> encode (byteSequence :: Leaf BareItem ByteSequence) (ByteSequence "pleasure.")
-- ":cGxlYXN1cmUu:"
-- >>> decode (byteSequence :: Leaf BareItem ByteSequence) "cGxlYXN1cmUu"
-- Left ParseError
--
-- prop> \bs -> leafPrintParse (decode (byteSequence :: Leaf BareItem ByteSequence)) (encode (byteSequence :: Leaf BareItem ByteSequence)) (ByteSequence bs)
instance HasLeaf BareItem ByteSequence where
    leaf = bareLeaf (fmap ByteSequence . parseByteSequence) (\(ByteSequence bs) -> renderByteSequence bs) (Info "string" (Just "byte-sequence"))

byteSequence :: (HasLeaf t ByteSequence) => Leaf t ByteSequence
byteSequence = leaf

-- | Identity leaf — @decode@ never fails, so both directions hold
--   unconditionally for any input.
--
-- prop> leafPrintParse (decode (leaf :: Leaf BareItem ByteString)) (encode (leaf :: Leaf BareItem ByteString)) x
-- prop> leafParsePrint (decode (leaf :: Leaf BareItem ByteString)) (encode (leaf :: Leaf BareItem ByteString)) (bs :: ByteString)
instance HasLeaf BareItem ByteString where
    leaf = bareLeaf Right id (Info "string" Nothing)

-- | Rounding to 3 decimal places can turn a small negative value into all
--   zeroes (e.g. @-1.0e-4@ formats as @\"-0.000\"@), which then decodes to
--   plain @0@ (no negative-zero representation) and re-encodes as
--   @\"0.000\"@ — breaking even the stable roundtrip. Strip the sign
--   whenever rounding leaves nothing but zeroes.
renderSfDecimal :: Scientific -> ByteString
renderSfDecimal = BS8.pack . dropNegZero . formatScientific Fixed (Just 3)
  where
    dropNegZero ('-' : rest) | all (`elem` ("0." :: String)) rest = rest
    dropNegZero s = s

parseSfDecimal :: ByteString -> Either Text Scientific
parseSfDecimal bs = case BS8.readInt bs of
    _ -> case reads (BS8.unpack bs) of
        [(s, "")] -> Right s
        _ -> Left ("invalid sf-decimal: " <> decodeUtf8Lenient bs)

-- | Always encodes with exactly 3 decimal places (@formatScientific Fixed
--   (Just 3)@), but decodes any valid decimal syntax — so neither
--   'leafPrintParse' nor 'leafParsePrint' hold in general (encoding loses
--   precision beyond 3 decimal places; decoding accepts non-canonical
--   inputs like @\"1.5\"@ that don't reprint to themselves). Use
--   'leafPrintParsePrint'\/'leafParsePrintParse' instead.
--
-- >>> encode (scientific :: Leaf BareItem Scientific) 1.5
-- "1.500"
-- >>> decode (scientific :: Leaf BareItem Scientific) "1.5"
-- Right 1.5
-- >>> decode (scientific :: Leaf BareItem Scientific) (encode (scientific :: Leaf BareItem Scientific) 1.5)
-- Right 1.5
--
-- prop> leafPrintParsePrint (decode (scientific :: Leaf BareItem Scientific)) (encode (scientific :: Leaf BareItem Scientific)) x
-- prop> leafParsePrintParse (decode (scientific :: Leaf BareItem Scientific)) (encode (scientific :: Leaf BareItem Scientific)) (bs :: ByteString)
instance HasLeaf BareItem Scientific where
    leaf = bareLeaf parseSfDecimal renderSfDecimal (Info "number" Nothing)

-- | Same lossy-encode\/lenient-decode caveat as 'Scientific' — see there.
--
-- prop> leafPrintParsePrint (decode (double :: Leaf BareItem Double)) (encode (double :: Leaf BareItem Double)) x
instance HasLeaf BareItem Double where
    leaf = bareLeaf (fmap toRealFloat . parseSfDecimal) (renderSfDecimal . realToFrac) (Info "number" (Just "double"))

-- | Same lossy-encode\/lenient-decode caveat as 'Scientific' — see there.
--
-- prop> leafPrintParsePrint (decode (float :: Leaf BareItem Float)) (encode (float :: Leaf BareItem Float)) x
instance HasLeaf BareItem Float where
    leaf = bareLeaf (fmap toRealFloat . parseSfDecimal) (renderSfDecimal . realToFrac) (Info "number" (Just "float"))

-- | Encoded as @\@\<posix-seconds\>@, truncated to whole seconds — encoding
--   is lossy (drops sub-second precision), so 'leafPrintParse' doesn't hold
--   for a 'UTCTime' with a fractional-second component; use
--   'leafPrintParsePrint'\/'leafParsePrintParse' instead, same caveat as
--   'Scientific'.
--
-- >>> decode (utcTime :: Leaf BareItem UTCTime) "@1000000000"
-- Right 2001-09-09 01:46:40 UTC
-- >>> encode (utcTime :: Leaf BareItem UTCTime) (posixSecondsToUTCTime 1000000000)
-- "@1000000000"
--
-- prop> leafPrintParsePrint (decode (utcTime :: Leaf BareItem UTCTime)) (encode (utcTime :: Leaf BareItem UTCTime)) x
-- prop> leafParsePrintParse (decode (utcTime :: Leaf BareItem UTCTime)) (encode (utcTime :: Leaf BareItem UTCTime)) (bs :: ByteString)
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
