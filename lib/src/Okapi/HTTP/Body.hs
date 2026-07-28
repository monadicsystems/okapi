
-- | The body codec shared by request and response bodies alike —
--   @'Body' 'Okapi.Tree.ForRequest'@\/@'Body' 'Okapi.Tree.ForResponse'@
--   are the two side instantiations. 'json'\/'jsonValue'\/'none'\/'base'
--   are free in the phantom @ctx@ and work unqualified for either side;
--   only 'form' (request-only) is pinned to a specific side, right at its
--   GADT constructor.
module Okapi.HTTP.Body (
    Body (..),
    Base,
    None (..),
    IsoJson,
    ParseError (..),
    parser,
    printer,
    try,
    base,
    json,
    jsonValue,
    form,
    none,
) where

import Control.Exception (Exception, Handler (..), catches, throwIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)
import Okapi.Tree (ForRequest)

-- $setup
-- >>> :set -XTypeApplications
-- >>> import Web.FormUrlEncoded (Form)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

-- | The value 'none' decodes\/encodes to — a dedicated singleton
--   rather than reusing @()@, so a no-body response reads as its own
--   distinct thing rather than an incidental unit value.
data None = None deriving (Eq, Show)

-- | Unlike the 'Okapi.Tree'-based DSLs, 'parser'\/'printer' here are
--   IO-wrapped and monadic ('Context' itself is @IO LBS.ByteString@) —
--   'parser' is total (it just transforms one @IO@ action into another; it
--   has no way to synchronously fail, so it carries no 'Either'). A real
--   decode failure only surfaces later, as a thrown IO exception, when the
--   returned action is actually run — a caller who wants to observe it
--   calls 'try' on that action themselves. The pure 'Okapi.Tree'
--   round-trip law vocabulary (@printParse@\/@parsePrint@, which compare
--   'Either' results directly) doesn't apply here. Concrete, executed
--   @>>>@ examples stand in for @prop>@ properties on this module. Named
--   @parser@\/@printer@, not @parse@\/@print@, specifically so doctests
--   here (and any caller) never have to fight @Prelude.print@ ambiguity.
type Body :: Type -> Type -> Type
data Body ctx a where
    Base       :: Body ctx Base
    Json      :: IsoJson a => Body ctx (IO a)
    JsonValue :: Body ctx (IO Aeson.Value)
    NoContent :: Body ctx (IO None)
    Form      :: (ToForm a, FromForm a) => Body ForRequest (IO a)

-- | What a body decode can fail with — a real sum type, not a flat
--   message, tagged by which decoder actually failed. 'JsonError' carries
--   'Aeson.eitherDecode's own error 'String'; 'FormError' carries
--   'Web.FormUrlEncoded.fromForm's own error 'Text' (what 'urlDecodeAsForm'
--   calls) — both are the real, native error types those functions expose,
--   not a flattening of something richer.
data ParseError
    = JsonError String
    | FormError Text
    deriving (Eq, Show)

newtype JsonDecodeException = JsonDecodeException String deriving (Show)
instance Exception JsonDecodeException

newtype FormDecodeException = FormDecodeException Text deriving (Show)
instance Exception FormDecodeException

jsonDecode :: IsoJson a => LBS.ByteString -> IO a
jsonDecode bs = case Aeson.eitherDecode bs of
    Left err -> throwIO (JsonDecodeException err)
    Right x  -> pure x

-- | Like 'jsonDecode', but for 'Aeson.Value' itself — no 'IsoJson'
--   constraint needed, since 'Aeson.Value's 'Aeson.FromJSON' instance is
--   unconditional.
jsonValueDecode :: LBS.ByteString -> IO Aeson.Value
jsonValueDecode bs = case Aeson.eitherDecode bs of
    Left err -> throwIO (JsonDecodeException err)
    Right v  -> pure v

formDecode :: FromForm a => LBS.ByteString -> IO a
formDecode bs = case urlDecodeAsForm bs of
    Left err -> throwIO (FormDecodeException err)
    Right x  -> pure x

parser :: Body ctx a -> IO LBS.ByteString -> a
parser Base       ioLbs = ioLbs
parser Json      ioLbs = ioLbs >>= jsonDecode
parser JsonValue ioLbs = ioLbs >>= jsonValueDecode
parser Form      ioLbs = ioLbs >>= formDecode
parser NoContent _     = pure None

printer :: Body ctx a -> a -> IO LBS.ByteString
printer Base       ioLbs = ioLbs
printer Json      ioA   = Aeson.encode <$> ioA
printer JsonValue ioA   = Aeson.encode <$> ioA
printer Form      ioA   = urlEncodeAsForm <$> ioA
printer NoContent _     = pure mempty

-- | Run an already-deferred body action (e.g. a 'Okapi.HTTP.Request.Data'\/
--   'Okapi.HTTP.Response.Data' value's own @body@ field), catching a
--   JSON\/form decode failure as a real 'ParseError' instead of an
--   uncaught exception — covers both 'json' and 'jsonValue' bodies (both
--   throw the same internal exception type on decode failure) as well as
--   'form'. Opt-in — 'parser' itself stays total\/unwrapped (see its own
--   haddock above); call 'try' explicitly wherever a decode failure needs
--   to be observed, e.g. @Body.try resData.body@ from a handler.
--
-- >>> try (parser (json @Int) (pure "42"))
-- Right 42
-- >>> try (parser (json @Int) (pure "not json"))
-- Left (JsonError "Unexpected \"not json\", expecting JSON value")
try :: IO a -> IO (Either ParseError a)
try act = (Right <$> act) `catches`
    [ Handler (\(JsonDecodeException err) -> pure (Left (JsonError err)))
    , Handler (\(FormDecodeException err) -> pure (Left (FormError err)))
    ]

-- | Pass the raw body bytes straight through, unconstrained.
--
-- >>> r1 <- parser base (pure "hello")
-- >>> r1
-- "hello"
-- >>> r2 <- printer base (pure "hello")
-- >>> r2
-- "hello"
base :: Body ctx (IO LBS.ByteString)
base = Base

-- | What 'base' decodes\/encodes to — the maximally unconstrained body slot,
--   shared by both sides.
type Base = IO LBS.ByteString

-- | Decode\/encode the body as JSON, into a typed value with 'Aeson.FromJSON'\/
--   'Aeson.ToJSON'\/'ToSchema' instances.
--
-- >>> r3 <- parser (json @Int) (pure "42")
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
-- >>> r <- parser jsonValue (pure "{\"a\":1}")
-- >>> r
-- Object (fromList [("a",Number 1.0)])
jsonValue :: Body ctx (IO Aeson.Value)
jsonValue = JsonValue

-- | Decode\/encode the body as @application\/x-www-form-urlencoded@.
--
-- >>> r5 <- parser (form @Form) (pure "a=1&b=2")
-- >>> r5
-- fromList [("a","1"),("b","2")]
-- >>> r6 <- printer (form @Form) (pure r5)
-- >>> r6
-- "a=1&b=2"
form :: (ToForm a, FromForm a) => Body ForRequest (IO a)
form = Form

-- | No body at all — parsing ignores the input, printing produces nothing.
--
-- >>> r7 <- parser none (pure "")
-- >>> r7
-- None
-- >>> r8 <- printer none (pure None)
-- >>> r8
-- ""
none :: Body ctx (IO None)
none = NoContent
