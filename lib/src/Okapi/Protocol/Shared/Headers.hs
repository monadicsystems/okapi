{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Shared.Headers (
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
    Separator (..),
    structHeader,
    structHeaderL,
    structHeaderSep,
    contentTypeHeader,
    HasHeaders (..),
    ConstF (..),
    CookieF (..),
    SetCookieF (..),
    GHeaders (..),
    headersCodec,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Kind (Type)
import Data.List (partition)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (D1, C1, S1, K1 (..), M1 (..), Rec0, (:*:) (..), Generic (..), Selector (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoHeaderData, IsoCookieData, parseHeader, toHeader, parseCookieValue, toCookieValue)
import Okapi.Protocol.Shared.Headers.Value (HeaderValue)
import Okapi.Protocol.Shared.Headers.Value qualified as HV
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
    -- | A header whose value has internal structure (a separator-joined list of
    --   items), decoded by a 'HeaderValue' sub-codec. e.g. @Content-Type@.
    Structured :: Separator -> HTTP.HeaderName -> Codec HeaderValue a a -> Headers ctx a

-- | Item separator for a structured header value.
data Separator = Semicolon | Comma deriving (Eq, Show)

sepChar :: Separator -> Char
sepChar Semicolon = ';'
sepChar Comma     = ','

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
    headersAlg (Structured sep name c) hs =
        case partition (\(k, _) -> k == name) hs of
            ([], _)            -> (Left ParseError, hs)
            ((_, v) : _, rest) ->
                case fst (HV.parse c (HV.splitItems (sepChar sep) v)) of
                    Left _  -> (Left ParseError, hs)
                    Right x -> (Right x, rest)

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
    headersPrinter (Structured sep name c) a   = [(name, HV.renderItems (sepChar sep) (HV.print c a))]

renderSC :: IsoCookieData a => ByteString -> a -> (HTTP.HeaderName, ByteString)
renderSC name x =
    let sc = Cookie.defaultSetCookie
              { Cookie.setCookieName  = name
              , Cookie.setCookieValue = toCookieValue x
              }
    in ("set-cookie", LBS.toStrict (Builder.toLazyByteString (Cookie.renderSetCookie sc)))

raw :: Codec (Headers ctx) HTTP.RequestHeaders HTTP.RequestHeaders
raw = Lift Raw

-- | Required request or response header by name; parsing fails if absent.
header :: IsoHeaderData a => HTTP.HeaderName -> Codec (Headers ctx) a a
header key = Lift (Header key)

-- | Optional header by name; yields 'Nothing' when the header is absent.
header' :: IsoHeaderData a => HTTP.HeaderName -> Codec (Headers ctx) (Maybe a) (Maybe a)
header' key = Lift (Header' key)

-- | Assert a fixed header key/value pair; invisible to the surrounding codec's decoded type.
header_ :: HTTP.HeaderName -> ByteString -> Codec (Headers ctx) h h -> Codec (Headers ctx) h h
header_ k v c = Apply (LMap (const ()) (FMap (const id) (Lift (Header_ k v)))) c

-- | Required request cookie by name; parsing fails if the cookie is absent.
cookie :: IsoCookieData a => ByteString -> Codec (Headers ForRequest) a a
cookie name = Lift (Cookie name)

-- | Optional request cookie; yields 'Nothing' when the cookie is absent.
cookie' :: IsoCookieData a => ByteString -> Codec (Headers ForRequest) (Maybe a) (Maybe a)
cookie' name = Lift (Cookie' name)

-- | Required @Set-Cookie@ response header by name.
setCookie :: IsoCookieData a => ByteString -> Codec (Headers ForResponse) a a
setCookie name = Lift (SetCookie name)

-- | Optional @Set-Cookie@ response header; yields 'Nothing' when absent.
setCookie' :: IsoCookieData a => ByteString -> Codec (Headers ForResponse) (Maybe a) (Maybe a)
setCookie' name = Lift (SetCookie' name)

-- | A header whose value is a structured, separator-joined list of items,
--   decoded by a 'HeaderValue' do-block. Choose the item separator explicitly.
--   The header must be present on parse.
structHeaderSep :: Separator -> HTTP.HeaderName -> Codec HeaderValue a a -> Codec (Headers ctx) a a
structHeaderSep sep name c = Lift (Structured sep name c)

-- | 'structHeaderSep' with a semicolon separator — Content-Type, HSTS, etc.
structHeader :: HTTP.HeaderName -> Codec HeaderValue a a -> Codec (Headers ctx) a a
structHeader = structHeaderSep Semicolon

-- | 'structHeaderSep' with a comma separator — flat list headers (Cache-Control, Vary, Allow).
structHeaderL :: HTTP.HeaderName -> Codec HeaderValue a a -> Codec (Headers ctx) a a
structHeaderL = structHeaderSep Comma

-- | Fold an (invisible) @Content-Type@ assertion for the given media-type token into an
--   existing headers codec — prints @Content-Type: <media>@ and, on parse, asserts the
--   media-type token is present (tolerating parameters like @; charset=utf-8@). The decoded
--   headers type is unchanged.
contentTypeHeader :: ByteString -> Codec (Headers ctx) h h -> Codec (Headers ctx) h h
contentTypeHeader mt hc =
    Apply (LMap (const ()) (FMap (const id) (structHeader "content-type" (HV.flag (decodeUtf8 mt))))) hc

class HasHeaders (contract :: Type -> Type -> Type) where
    type Ctx contract :: Type
    headers ::
        Codec (Headers (Ctx contract)) h h ->
        contract [HTTP.Header] b ->
        contract h b


-- ── Generic headers deriving ──────────────────────────────────────────────────

-- | Record field type for a constant-value header assertion.
--   @val@ is the required header value; the header name comes from the field name (with @_@ → @-@).
--   The field contributes no value to the decoded type — it only validates presence.
data ConstF (val :: Symbol) = ConstF deriving (Eq, Show)

-- | Record field type for a request cookie. The field name (with @_@ → @-@) becomes the cookie name.
newtype CookieF a = CookieF { getCookieF :: a } deriving (Eq, Show)

-- | Record field type for a response Set-Cookie. The field name (with @_@ → @-@) becomes the cookie name.
newtype SetCookieF a = SetCookieF { getSetCookieF :: a } deriving (Eq, Show)

fieldToHeaderName :: String -> HTTP.HeaderName
fieldToHeaderName = CI.mk . encodeUtf8 . Text.pack . map (\c -> if c == '_' then '-' else c)

fieldToCookieName :: String -> ByteString
fieldToCookieName = encodeUtf8 . Text.pack . map (\c -> if c == '_' then '-' else c)

class GHeaders (ctx :: Type) (f :: Type -> Type) where
    gHeadersCodec :: Codec (Headers ctx) (f ()) (f ())

instance GHeaders ctx f => GHeaders ctx (D1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance GHeaders ctx f => GHeaders ctx (C1 meta f) where
    gHeadersCodec = FMap M1 $ LMap unM1 gHeadersCodec

instance (GHeaders ctx f, GHeaders ctx g) => GHeaders ctx (f :*: g) where
    gHeadersCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gHeadersCodec))
            (LMap (\(_ :*: r) -> r) gHeadersCodec)

instance (Selector s, IsoHeaderData a) => GHeaders ctx (S1 s (Rec0 a)) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 a) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Header key)

instance {-# OVERLAPPING #-} (Selector s, IsoHeaderData a) => GHeaders ctx (S1 s (Rec0 (Maybe a))) where
    gHeadersCodec =
        let key = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Header' key)

instance {-# OVERLAPPING #-} (Selector s, IsoCookieData a) => GHeaders ForRequest (S1 s (Rec0 (CookieF a))) where
    gHeadersCodec =
        let name = fieldToCookieName (selName (undefined :: S1 s (Rec0 (CookieF a)) ()))
        in FMap (M1 . K1 . CookieF) $ LMap (getCookieF . unK1 . unM1) $ Lift (Cookie name)

instance {-# OVERLAPPING #-} (Selector s, IsoCookieData a) => GHeaders ForRequest (S1 s (Rec0 (Maybe (CookieF a)))) where
    gHeadersCodec =
        let name = fieldToCookieName (selName (undefined :: S1 s (Rec0 (Maybe (CookieF a))) ()))
        in FMap (M1 . K1 . fmap CookieF) $ LMap (fmap getCookieF . unK1 . unM1) $ Lift (Cookie' name)

instance {-# OVERLAPPING #-} (Selector s, IsoCookieData a) => GHeaders ForResponse (S1 s (Rec0 (SetCookieF a))) where
    gHeadersCodec =
        let name = fieldToCookieName (selName (undefined :: S1 s (Rec0 (SetCookieF a)) ()))
        in FMap (M1 . K1 . SetCookieF) $ LMap (getSetCookieF . unK1 . unM1) $ Lift (SetCookie name)

instance {-# OVERLAPPING #-} (Selector s, IsoCookieData a) => GHeaders ForResponse (S1 s (Rec0 (Maybe (SetCookieF a)))) where
    gHeadersCodec =
        let name = fieldToCookieName (selName (undefined :: S1 s (Rec0 (Maybe (SetCookieF a))) ()))
        in FMap (M1 . K1 . fmap SetCookieF) $ LMap (fmap getSetCookieF . unK1 . unM1) $ Lift (SetCookie' name)

instance {-# OVERLAPPING #-} (Selector s, KnownSymbol val) => GHeaders ctx (S1 s (Rec0 (ConstF val))) where
    gHeadersCodec =
        let k = fieldToHeaderName (selName (undefined :: S1 s (Rec0 (ConstF val)) ()))
            v = encodeUtf8 (Text.pack (symbolVal (Proxy @val)))
        in FMap (\() -> M1 (K1 ConstF)) $ LMap (const ()) $ Lift (Header_ k v)

-- | Build a 'Headers' codec from a Generic record type.
--   Field types determine which constructor is used:
--   @a@ → required header, @Maybe a@ → optional header,
--   @CookieF a@ → request cookie, @SetCookieF a@ → response Set-Cookie,
--   @ConstF val@ → constant-value assertion (field name → header key, @val@ → required value).
--   Field names (with @_@ converted to @-@) become header/cookie names.
headersCodec :: forall ctx a. (Generic a, GHeaders ctx (Rep a)) => Codec (Headers ctx) a a
headersCodec = FMap (to @a) $ LMap (from @a) gHeadersCodec
