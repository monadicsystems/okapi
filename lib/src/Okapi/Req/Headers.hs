{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Headers (
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    header,
    header',
    cookie,
    cookie',
    withHeader,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.List (partition)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoHeaderData, IsoCookieData, parseHeader, toHeader, parseCookieValue, toCookieValue)
import Prelude hiding (print)
import Web.Cookie (parseCookies)

type Headers :: Type -> Type
data Headers a where
    Raw       :: Headers HTTP.RequestHeaders
    Header    :: IsoHeaderData a => HTTP.HeaderName -> Headers a
    HeaderOpt :: IsoHeaderData a => HTTP.HeaderName -> Headers (Maybe a)
    Cookie    :: IsoCookieData a => ByteString -> Headers a
    CookieOpt :: IsoCookieData a => ByteString -> Headers (Maybe a)
    Lit       :: HTTP.HeaderName -> ByteString -> Headers ()

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Headers = HTTP.RequestHeaders
type instance ParseErrorOf Headers = ParseError

parse :: Codec Headers i o -> HTTP.RequestHeaders -> (Either ParseError o, HTTP.RequestHeaders)
parse = Codec.parser headersAlg
  where
    headersAlg :: forall a. Headers a -> HTTP.RequestHeaders -> (Either ParseError a, HTTP.RequestHeaders)
    headersAlg Raw hs = (Right hs, [])
    headersAlg (Header key) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)             -> (Left ParseError, hs)
            ((_, v) : _, rest)  -> case parseHeader v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    headersAlg (HeaderOpt key) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)             -> (Right Nothing, hs)
            ((_, v) : _, rest)  -> case parseHeader v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    headersAlg (Cookie name) hs =
        let allCookieBS           = BS.intercalate "; " [v | ("cookie", v) <- hs]
            cookies               = parseCookies allCookieBS
            (found, remaining)    = partition (\(k, _) -> k == name) cookies
            stripped              = filter (\(k, _) -> k /= "cookie") hs
            reEncoded             = BS.intercalate "; " (map (\(k, v) -> k <> "=" <> v) remaining)
            newHs                 = if BS.null reEncoded then stripped else ("cookie", reEncoded) : stripped
        in case found of
            []           -> (Left ParseError, hs)
            ((_, v) : _) -> case parseCookieValue v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, newHs)
    headersAlg (CookieOpt name) hs =
        let allCookieBS           = BS.intercalate "; " [v | ("cookie", v) <- hs]
            cookies               = parseCookies allCookieBS
            (found, remaining)    = partition (\(k, _) -> k == name) cookies
            stripped              = filter (\(k, _) -> k /= "cookie") hs
            reEncoded             = BS.intercalate "; " (map (\(k, v) -> k <> "=" <> v) remaining)
            newHs                 = if BS.null reEncoded then stripped else ("cookie", reEncoded) : stripped
        in case found of
            []           -> (Right Nothing, hs)
            ((_, v) : _) -> case parseCookieValue v of
                Left _  -> (Right Nothing, newHs)
                Right x -> (Right (Just x), newHs)
    headersAlg (Lit k v) hs =
        case lookup k hs of
            Just v' | v' == v -> (Right (), filter (\(k', _) -> k' /= k) hs)
            _                  -> (Left ParseError, hs)

print :: Codec Headers i o -> i -> HTTP.RequestHeaders
print = Codec.printer headersPrinter
  where
    headersPrinter :: forall a. Headers a -> a -> HTTP.RequestHeaders
    headersPrinter Raw hs                   = hs
    headersPrinter (Header key) x           = [(key, toHeader x)]
    headersPrinter (HeaderOpt _) Nothing    = []
    headersPrinter (HeaderOpt key) (Just x) = [(key, toHeader x)]
    headersPrinter (Cookie name) x          = [("cookie", name <> "=" <> toCookieValue x)]
    headersPrinter (CookieOpt _) Nothing    = []
    headersPrinter (CookieOpt name) (Just x) = [("cookie", name <> "=" <> toCookieValue x)]
    headersPrinter (Lit k v) ()             = [(k, v)]

raw :: Codec Headers HTTP.RequestHeaders HTTP.RequestHeaders
raw = Embed Raw

header :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers a a
header key = Embed (Header key)

header' :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers (Maybe a) (Maybe a)
header' key = Embed (HeaderOpt key)

cookie :: IsoCookieData a => ByteString -> Codec Headers a a
cookie name = Embed (Cookie name)

cookie' :: IsoCookieData a => ByteString -> Codec Headers (Maybe a) (Maybe a)
cookie' name = Embed (CookieOpt name)

withHeader :: HTTP.HeaderName -> ByteString -> Codec Headers h h -> Codec Headers h h
withHeader k v c = Apply (LMap (const ()) (FMap (const id) (Embed (Lit k v)))) c
