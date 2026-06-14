{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Headers (
    ForRequest,
    ForResponse,
    Headers (..),
    ParseError (..),
    parse,
    print,
    raw,
    header,
    header',
    header_,
    cookie,
    cookie',
    setCookie,
    setCookie',
    HasHeaders (..),
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
import Web.Cookie (parseCookies)

data ForRequest
data ForResponse

type Headers :: Type -> Type -> Type
data Headers ctx a where
    Raw      :: Headers ctx HTTP.RequestHeaders
    Header   :: IsoHeaderData a => HTTP.HeaderName -> Headers ctx a
    Header'  :: IsoHeaderData a => HTTP.HeaderName -> Headers ctx (Maybe a)
    Header_  :: HTTP.HeaderName -> ByteString -> Headers ctx ()
    Cookie   :: IsoCookieData a => ByteString -> Headers ForRequest a
    Cookie'  :: IsoCookieData a => ByteString -> Headers ForRequest (Maybe a)
    SetCookie  :: IsoCookieData a => ByteString -> Headers ForResponse a
    SetCookie' :: IsoCookieData a => ByteString -> Headers ForResponse (Maybe a)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf      (Headers ctx) = [HTTP.Header]
type instance ParseErrorOf (Headers ctx) = ParseError

parse :: forall ctx i o. Codec (Headers ctx) i o -> [HTTP.Header] -> (Either ParseError o, [HTTP.Header])
parse = Codec.parser headersAlg
  where
    headersAlg :: forall a. Headers ctx a -> HTTP.RequestHeaders -> (Either ParseError a, HTTP.RequestHeaders)
    headersAlg Raw hs = (Right hs, [])
    headersAlg (Header key) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) -> case parseHeader v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, rest)
    headersAlg (Header' key) hs =
        case partition (\(k, _) -> k == key) hs of
            ([], _)            -> (Right Nothing, hs)
            ((_, v) : _, rest) -> case parseHeader v of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    headersAlg (Header_ k v) hs =
        case lookup k hs of
            Just v' | v' == v -> (Right (), filter (\(k', _) -> k' /= k) hs)
            _                 -> (Left ParseError, hs)
    headersAlg (Cookie name) hs =
        let allCookieBS        = BS.intercalate "; " [v | ("cookie", v) <- hs]
            cookies            = parseCookies allCookieBS
            (found, remaining) = partition (\(k, _) -> k == name) cookies
            stripped           = filter (\(k, _) -> k /= "cookie") hs
            reEncoded          = BS.intercalate "; " (map (\(k, v) -> k <> "=" <> v) remaining)
            newHs              = if BS.null reEncoded then stripped else ("cookie", reEncoded) : stripped
        in case found of
            []           -> (Left ParseError, hs)
            ((_, v) : _) -> case parseCookieValue v of
                Left _  -> (Left ParseError, hs)
                Right x -> (Right x, newHs)
    headersAlg (Cookie' name) hs =
        let allCookieBS        = BS.intercalate "; " [v | ("cookie", v) <- hs]
            cookies            = parseCookies allCookieBS
            (found, remaining) = partition (\(k, _) -> k == name) cookies
            stripped           = filter (\(k, _) -> k /= "cookie") hs
            reEncoded          = BS.intercalate "; " (map (\(k, v) -> k <> "=" <> v) remaining)
            newHs              = if BS.null reEncoded then stripped else ("cookie", reEncoded) : stripped
        in case found of
            []           -> (Right Nothing, hs)
            ((_, v) : _) -> case parseCookieValue v of
                Left _  -> (Right Nothing, newHs)
                Right x -> (Right (Just x), newHs)
    headersAlg (SetCookie name) hs =
        case partition (isSetCookieFor name) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) ->
                let val = Cookie.setCookieValue (Cookie.parseSetCookie v)
                in case parseCookieValue val of
                    Left _  -> (Left ParseError, hs)
                    Right x -> (Right x, rest)
    headersAlg (SetCookie' name) hs =
        case partition (isSetCookieFor name) hs of
            ([], _)            -> (Right Nothing, hs)
            ((_, v) : _, rest) ->
                let val = Cookie.setCookieValue (Cookie.parseSetCookie v)
                in case parseCookieValue val of
                    Left _  -> (Right Nothing, rest)
                    Right x -> (Right (Just x), rest)

isSetCookieFor :: ByteString -> (HTTP.HeaderName, ByteString) -> Bool
isSetCookieFor name ("set-cookie", v) =
    Cookie.setCookieName (Cookie.parseSetCookie v) == name
isSetCookieFor _ _ = False

print :: forall ctx i o. Codec (Headers ctx) i o -> i -> [HTTP.Header]
print = Codec.printer headersPrinter
  where
    headersPrinter :: forall a. Headers ctx a -> a -> HTTP.RequestHeaders
    headersPrinter Raw hs                      = hs
    headersPrinter (Header key) x              = [(key, toHeader x)]
    headersPrinter (Header' _) Nothing         = []
    headersPrinter (Header' key) (Just x)      = [(key, toHeader x)]
    headersPrinter (Header_ k v) ()            = [(k, v)]
    headersPrinter (Cookie name) x             = [("cookie", name <> "=" <> toCookieValue x)]
    headersPrinter (Cookie' _) Nothing         = []
    headersPrinter (Cookie' name) (Just x)     = [("cookie", name <> "=" <> toCookieValue x)]
    headersPrinter (SetCookie name) x          = [renderSC name x]
    headersPrinter (SetCookie' _) Nothing      = []
    headersPrinter (SetCookie' name) (Just x)  = [renderSC name x]

renderSC :: IsoCookieData a => ByteString -> a -> (HTTP.HeaderName, ByteString)
renderSC name x =
    let sc = Cookie.defaultSetCookie
              { Cookie.setCookieName  = name
              , Cookie.setCookieValue = toCookieValue x
              }
    in ("set-cookie", LBS.toStrict (Builder.toLazyByteString (Cookie.renderSetCookie sc)))

raw :: Codec (Headers ctx) HTTP.RequestHeaders HTTP.RequestHeaders
raw = Embed Raw

header :: IsoHeaderData a => HTTP.HeaderName -> Codec (Headers ctx) a a
header key = Embed (Header key)

header' :: IsoHeaderData a => HTTP.HeaderName -> Codec (Headers ctx) (Maybe a) (Maybe a)
header' key = Embed (Header' key)

header_ :: HTTP.HeaderName -> ByteString -> Codec (Headers ctx) h h -> Codec (Headers ctx) h h
header_ k v c = Apply (LMap (const ()) (FMap (const id) (Embed (Header_ k v)))) c

cookie :: IsoCookieData a => ByteString -> Codec (Headers ForRequest) a a
cookie name = Embed (Cookie name)

cookie' :: IsoCookieData a => ByteString -> Codec (Headers ForRequest) (Maybe a) (Maybe a)
cookie' name = Embed (Cookie' name)

setCookie :: IsoCookieData a => ByteString -> Codec (Headers ForResponse) a a
setCookie name = Embed (SetCookie name)

setCookie' :: IsoCookieData a => ByteString -> Codec (Headers ForResponse) (Maybe a) (Maybe a)
setCookie' name = Embed (SetCookie' name)

class HasHeaders (contract :: Type -> Type -> Type) where
    type Ctx contract :: Type
    headers ::
        Codec (Headers (Ctx contract)) h h ->
        contract [HTTP.Header] b ->
        contract h b
