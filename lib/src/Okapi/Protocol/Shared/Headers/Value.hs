{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | A sub-DSL for the /internal structure of a single header value/.
--
-- A structured header value is a separator-joined list of items, each either a
-- bare token (@application\/json@, @no-cache@) or a @key=value@ parameter
-- (@charset=utf-8@). This module is separator-agnostic: it operates on already
-- split items ('Items'); the splitting/joining (and the separator choice) is
-- done by the caller (see "Okapi.Protocol.Shared.Headers" @structHeader@) via
-- 'splitItems' / 'renderItems'.
module Okapi.Protocol.Shared.Headers.Value (
    HeaderValue (..),
    Item,
    Items,
    ParseError (..),
    parse,
    print,
    param,
    param',
    flag,
    flag',
    raw,
    splitItems,
    renderItems,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Kind (Type)
import Data.List (partition)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoHeaderData, parseHeader, toHeader)
import Prelude hiding (print)

-- | One item of a structured header value: a case-insensitive key, and an
--   optional value (@Nothing@ for a bare token, @Just@ for @key=value@).
type Item = (CI ByteString, Maybe ByteString)

type Items = [Item]

type HeaderValue :: Type -> Type
data HeaderValue a where
    -- | Required @key=value@ parameter (or bare token whose value you parse).
    Param  :: IsoHeaderData a => Text -> HeaderValue a
    -- | Optional @key=value@ parameter; 'Nothing' when absent.
    Param' :: IsoHeaderData a => Text -> HeaderValue (Maybe a)
    -- | Required bare token (presence-only); e.g. the media type @application/json@.
    Flag   :: Text -> HeaderValue ()
    -- | Optional bare token.
    Flag'  :: Text -> HeaderValue (Maybe ())
    -- | The whole (remaining) item list, unparsed.
    Raw    :: HeaderValue Items

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf      HeaderValue = Items
type instance ParseErrorOf HeaderValue = ParseError

-- | Parse a structured value codec against an already-split item list.
--   Unconsumed items are dropped (lenient — e.g. an ignored @charset@).
parse :: Codec HeaderValue i o -> Items -> (Either ParseError o, Items)
parse = Codec.parser alg
  where
    alg :: forall a. HeaderValue a -> Items -> (Either ParseError a, Items)
    alg (Param key) items =
        let k = CI.mk (encodeUtf8 key)
        in case partition (\(ik, _) -> ik == k) items of
            ([], _)                  -> (Left ParseError, items)
            ((_, Nothing) : _, _)    -> (Left ParseError, items)
            ((_, Just v) : _, rest)  -> case parseHeader v of
                Left _  -> (Left ParseError, items)
                Right x -> (Right x, rest)
    alg (Param' key) items =
        let k = CI.mk (encodeUtf8 key)
        in case partition (\(ik, _) -> ik == k) items of
            ([], _)                   -> (Right Nothing, items)
            ((_, Nothing) : _, rest)  -> (Right Nothing, rest)
            ((_, Just v) : _, rest)   -> case parseHeader v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    alg (Flag key) items =
        let k = CI.mk (encodeUtf8 key)
        in case partition (\(ik, _) -> ik == k) items of
            ([], _)       -> (Left ParseError, items)
            (_ : _, rest) -> (Right (), rest)
    alg (Flag' key) items =
        let k = CI.mk (encodeUtf8 key)
        in case partition (\(ik, _) -> ik == k) items of
            ([], _)       -> (Right Nothing, items)
            (_ : _, rest) -> (Right (Just ()), rest)
    alg Raw items = (Right items, [])

-- | Render a structured value codec to its item list.
print :: Codec HeaderValue i o -> i -> Items
print = Codec.printer pr
  where
    pr :: forall a. HeaderValue a -> a -> Items
    pr (Param key) x          = [(CI.mk (encodeUtf8 key), Just (toHeader x))]
    pr (Param' _) Nothing     = []
    pr (Param' key) (Just x)  = [(CI.mk (encodeUtf8 key), Just (toHeader x))]
    pr (Flag key) ()          = [(CI.mk (encodeUtf8 key), Nothing)]
    pr (Flag' key) (Just ())  = [(CI.mk (encodeUtf8 key), Nothing)]
    pr (Flag' _) Nothing      = []
    pr Raw items              = items

-- | Required @key=value@ parameter.
param :: IsoHeaderData a => Text -> Codec HeaderValue a a
param key = Lift (Param key)

-- | Optional @key=value@ parameter; yields 'Nothing' when absent.
param' :: IsoHeaderData a => Text -> Codec HeaderValue (Maybe a) (Maybe a)
param' key = Lift (Param' key)

-- | Required bare token (e.g. assert the media type @application/json@).
flag :: Text -> Codec HeaderValue () ()
flag key = Lift (Flag key)

-- | Optional bare token; yields 'Just ()' when present.
flag' :: Text -> Codec HeaderValue (Maybe ()) (Maybe ())
flag' key = Lift (Flag' key)

-- | The whole remaining item list, unparsed.
raw :: Codec HeaderValue Items Items
raw = Lift Raw


-- ── Split / join (quote-aware) ─────────────────────────────────────────────────

-- | Split a raw header value into items on the given separator, ignoring
--   separators inside double-quoted strings; trims whitespace and unquotes values.
splitItems :: Char -> ByteString -> Items
splitItems sep = map toItem . filter (not . BS.null) . map strip . splitTop sep
  where
    toItem chunk = case BS8.elemIndex '=' chunk of
        Nothing -> (CI.mk chunk, Nothing)
        Just i  ->
            let k = strip (BS.take i chunk)
                v = unquote (strip (BS.drop (i + 1) chunk))
            in (CI.mk k, Just v)

splitTop :: Char -> ByteString -> [ByteString]
splitTop sep bs =
    let sepW = fromIntegral (fromEnum sep)
        (_, cur, acc) = BS.foldl' (step sepW) (False, BS.empty, []) bs
    in reverse (cur : acc)
  where
    step sepW (inQ, cur, acc) w
        | w == 34            = (not inQ, BS.snoc cur w, acc)   -- '"'
        | w == sepW && not inQ = (inQ, BS.empty, cur : acc)
        | otherwise          = (inQ, BS.snoc cur w, acc)

-- | Render items back to a raw header value, joined by the separator (with a
--   trailing space), quoting values that contain the separator/space/quotes.
renderItems :: Char -> Items -> ByteString
renderItems sep items = BS.intercalate (BS8.pack [sep, ' ']) (map render items)
  where
    render (k, Nothing) = CI.original k
    render (k, Just v)  = CI.original k <> "=" <> quoteIfNeeded v
    quoteIfNeeded v
        | BS.any needsQuote v = "\"" <> v <> "\""
        | otherwise           = v
    needsQuote w = w == 32 || w == 9 || w == 34 || w == 44 || w == 59
                || w == fromIntegral (fromEnum sep)

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

-- | Strip surrounding double quotes from a parameter value, if present.
unquote :: ByteString -> ByteString
unquote v
    | BS.length v >= 2 && BS.head v == 34 && BS.last v == 34 = BS.init (BS.drop 1 v)
    | otherwise = v
