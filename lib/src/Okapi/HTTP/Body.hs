
-- | The body codec shared by request and response bodies alike —
--   'Okapi.HTTP.Request.Body.RequestBody' and
--   'Okapi.HTTP.Response.Body.ResponseBody' are just type aliases for
--   @'Body' 'Okapi.HTTP.Tree.ForRequest'@/@'Body' 'Okapi.HTTP.Tree.ForResponse'@.
--   'json'\/'jsonValue'\/'none'\/'raw' are free in the phantom @ctx@ and
--   work unqualified for either side; only 'form' (request-only) is pinned
--   to a specific side, right at its GADT constructor.
module Okapi.HTTP.Body (
    Body (..),
    None (..),
    IsoJson,
    ParseError (..),
    parser,
    printer,
    raw,
    json,
    jsonValue,
    form,
    none,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)
import Okapi.HTTP.Tree (ForRequest)

-- $setup
-- >>> :set -XTypeApplications
-- >>> import Web.FormUrlEncoded (Form)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

-- | The value 'none' decodes\/encodes to — a dedicated singleton
--   rather than reusing @()@, so a no-body response reads as its own
--   distinct thing rather than an incidental unit value.
data None = None deriving (Eq, Show)

-- | Unlike the 'Okapi.HTTP.Tree'-based DSLs, 'parser'\/'printer' here are
--   IO-wrapped and monadic ('Context' itself is @IO LBS.ByteString@) —
--   'parser' never synchronously fails (every case is a bare 'Right'; a
--   real decode failure only surfaces later, as an IO exception, when the
--   returned action is actually run), so the pure 'Okapi.HTTP.Tree' round-trip
--   law vocabulary (@printParse@\/@parsePrint@, which compare 'Either'
--   results directly) doesn't apply here. Concrete, executed @>>>@
--   examples stand in for @prop>@ properties on this module. Named
--   @parser@\/@printer@, not @parse@\/@print@, specifically so doctests
--   here (and any caller) never have to fight @Prelude.print@ ambiguity.
type Body :: Type -> Type -> Type
data Body ctx a where
    Raw       :: Body ctx (IO LBS.ByteString)
    Json      :: IsoJson a => Body ctx (IO a)
    JsonValue :: Body ctx (IO Aeson.Value)
    NoContent :: Body ctx (IO None)
    Form      :: (ToForm a, FromForm a) => Body ForRequest (IO a)

data ParseError = ParseError deriving (Eq, Show)

jsonDecode :: IsoJson a => LBS.ByteString -> IO a
jsonDecode bs = case Aeson.eitherDecode bs of
    Left err -> fail err
    Right x  -> pure x

-- | Like 'jsonDecode', but for 'Aeson.Value' itself — no 'IsoJson'
--   constraint needed, since 'Aeson.Value's 'Aeson.FromJSON' instance is
--   unconditional.
jsonValueDecode :: LBS.ByteString -> IO Aeson.Value
jsonValueDecode bs = case Aeson.eitherDecode bs of
    Left err -> fail err
    Right v  -> pure v

formDecode :: FromForm a => LBS.ByteString -> IO a
formDecode bs = case urlDecodeAsForm bs of
    Left err -> fail (show err)
    Right x  -> pure x

parser :: Body ctx a -> IO LBS.ByteString -> Either ParseError a
parser Raw       ioLbs = Right ioLbs
parser Json      ioLbs = Right (ioLbs >>= jsonDecode)
parser JsonValue ioLbs = Right (ioLbs >>= jsonValueDecode)
parser Form      ioLbs = Right (ioLbs >>= formDecode)
parser NoContent _     = Right (pure None)

printer :: Body ctx a -> a -> IO LBS.ByteString
printer Raw       ioLbs = ioLbs
printer Json      ioA   = Aeson.encode <$> ioA
printer JsonValue ioA   = Aeson.encode <$> ioA
printer Form      ioA   = urlEncodeAsForm <$> ioA
printer NoContent _     = pure mempty

-- | Pass the raw body bytes straight through, unconstrained.
--
-- >>> r1 <- either (error . show) id (parser raw (pure "hello"))
-- >>> r1
-- "hello"
-- >>> r2 <- printer raw (pure "hello")
-- >>> r2
-- "hello"
raw :: Body ctx (IO LBS.ByteString)
raw = Raw

-- | Decode\/encode the body as JSON, into a typed value with 'Aeson.FromJSON'\/
--   'Aeson.ToJSON'\/'ToSchema' instances.
--
-- >>> r3 <- either (error . show) id (parser (json @Int) (pure "42"))
-- >>> r3
-- 42
-- >>> r4 <- printer (json @Int) (pure 42)
-- >>> r4
-- "42"
json :: IsoJson a => Body ctx (IO a)
json = Json

-- | Decode\/encode the body as a structured 'Aeson.Value', with no
--   'IsoJson' instances required — an escape hatch for callers who want to
--   work with dynamic\/untyped JSON without defining a domain type for it.
--
-- >>> r <- either (error . show) id (parser jsonValue (pure "{\"a\":1}"))
-- >>> r
-- Object (fromList [("a",Number 1.0)])
jsonValue :: Body ctx (IO Aeson.Value)
jsonValue = JsonValue

-- | Decode\/encode the body as @application\/x-www-form-urlencoded@.
--
-- >>> r5 <- either (error . show) id (parser (form @Form) (pure "a=1&b=2"))
-- >>> r5
-- fromList [("a","1"),("b","2")]
-- >>> r6 <- printer (form @Form) (pure r5)
-- >>> r6
-- "a=1&b=2"
form :: (ToForm a, FromForm a) => Body ForRequest (IO a)
form = Form

-- | No body at all — parsing ignores the input, printing produces nothing.
--
-- >>> r7 <- either (error . show) id (parser none (pure ""))
-- >>> r7
-- None
-- >>> r8 <- printer none (pure None)
-- >>> r8
-- ""
none :: Body ctx (IO None)
none = NoContent
