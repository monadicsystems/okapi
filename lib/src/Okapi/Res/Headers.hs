{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Res.Headers (
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    header,
    header',
    setCookie,
    setCookie',
    withHeader,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.List (partition)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoHeaderData, IsoCookieData, parseHeader, toHeader, parseCookieValue, toCookieValue)
import Prelude hiding (print)
import Web.Cookie qualified as Cookie

type Headers :: Type -> Type
data Headers a where
    Raw          :: Headers HTTP.ResponseHeaders
    Header       :: IsoHeaderData a => HTTP.HeaderName -> Headers a
    HeaderOpt    :: IsoHeaderData a => HTTP.HeaderName -> Headers (Maybe a)
    SetCookie    :: IsoCookieData a => ByteString -> Headers a
    SetCookieOpt :: IsoCookieData a => ByteString -> Headers (Maybe a)
    Lit          :: HTTP.HeaderName -> ByteString -> Headers ()

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Headers = HTTP.ResponseHeaders
type instance ParseErrorOf Headers = ParseError

parse :: Codec Headers i o -> HTTP.ResponseHeaders -> (Either ParseError o, HTTP.ResponseHeaders)
parse = Codec.parser resHeadersAlg
  where
    resHeadersAlg :: forall a. Headers a -> HTTP.ResponseHeaders -> (Either ParseError a, HTTP.ResponseHeaders)
    resHeadersAlg Raw hs = (Right hs, [])
    resHeadersAlg (Header key) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)             -> (Left ParseError, hs)
            ((_, v) : _, rest)  -> case parseHeader v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    resHeadersAlg (HeaderOpt key) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)             -> (Right Nothing, hs)
            ((_, v) : _, rest)  -> case parseHeader v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    resHeadersAlg (SetCookie name) hs =
        case partition (isSetCookieFor name) hs of
            ([], _)             -> (Left ParseError, hs)
            ((_, v) : _, rest)  ->
                let val = Cookie.setCookieValue (Cookie.parseSetCookie v)
                in case parseCookieValue val of
                    Left _  -> (Left ParseError, hs)
                    Right x -> (Right x, rest)
    resHeadersAlg (SetCookieOpt name) hs =
        case partition (isSetCookieFor name) hs of
            ([], _)             -> (Right Nothing, hs)
            ((_, v) : _, rest)  ->
                let val = Cookie.setCookieValue (Cookie.parseSetCookie v)
                in case parseCookieValue val of
                    Left _  -> (Right Nothing, rest)
                    Right x -> (Right (Just x), rest)
    resHeadersAlg (Lit k v) hs =
        case lookup k hs of
            Just v' | v' == v -> (Right (), filter (\(k', _) -> k' /= k) hs)
            _                  -> (Left ParseError, hs)

isSetCookieFor :: ByteString -> (HTTP.HeaderName, ByteString) -> Bool
isSetCookieFor name ("set-cookie", v) =
    Cookie.setCookieName (Cookie.parseSetCookie v) == name
isSetCookieFor _ _ = False

print :: Codec Headers i o -> i -> HTTP.ResponseHeaders
print = Codec.printer resHeadersPrinter
  where
    resHeadersPrinter :: forall a. Headers a -> a -> HTTP.ResponseHeaders
    resHeadersPrinter Raw hs                    = hs
    resHeadersPrinter (Header key) x            = [(key, toHeader x)]
    resHeadersPrinter (HeaderOpt _) Nothing     = []
    resHeadersPrinter (HeaderOpt key) (Just x)  = [(key, toHeader x)]
    resHeadersPrinter (SetCookie name) x        = [renderSC name x]
    resHeadersPrinter (SetCookieOpt _) Nothing  = []
    resHeadersPrinter (SetCookieOpt name) (Just x) = [renderSC name x]
    resHeadersPrinter (Lit k v) ()               = [(k, v)]

renderSC :: IsoCookieData a => ByteString -> a -> (HTTP.HeaderName, ByteString)
renderSC name x =
    let sc = Cookie.defaultSetCookie
              { Cookie.setCookieName  = name
              , Cookie.setCookieValue = toCookieValue x
              }
    in ("set-cookie", LBS.toStrict (Builder.toLazyByteString (Cookie.renderSetCookie sc)))

raw :: Codec Headers HTTP.ResponseHeaders HTTP.ResponseHeaders
raw = Embed Raw

header :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers a a
header key = Embed (Header key)

header' :: IsoHeaderData a => HTTP.HeaderName -> Codec Headers (Maybe a) (Maybe a)
header' key = Embed (HeaderOpt key)

setCookie :: IsoCookieData a => ByteString -> Codec Headers a a
setCookie name = Embed (SetCookie name)

setCookie' :: IsoCookieData a => ByteString -> Codec Headers (Maybe a) (Maybe a)
setCookie' name = Embed (SetCookieOpt name)

withHeader :: HTTP.HeaderName -> ByteString -> Codec Headers h h -> Codec Headers h h
withHeader k v c = Apply (LMap (const ()) (FMap (const id) (Embed (Lit k v)))) c
