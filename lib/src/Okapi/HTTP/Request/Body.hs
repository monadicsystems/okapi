
module Okapi.HTTP.Request.Body (
    Body (..),
    IsoJson,
    ParseError (..),
    parser,
    printer,
    raw,
    json,
    form,
    noContent,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Okapi.Tree (Failure, Context)
import Okapi.HTTP.Body (IsoJson, jsonDecode)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

-- $setup
-- >>> :set -XTypeApplications
-- >>> import Web.FormUrlEncoded (Form)

-- | Unlike the 'Okapi.Tree'-based DSLs, 'parser'\/'printer' here are
--   IO-wrapped and monadic ('Context' itself is @IO LBS.ByteString@) —
--   'parser' never synchronously fails (every case is a bare 'Right'; a
--   real decode failure only surfaces later, as an IO exception, when the
--   returned action is actually run), so the pure 'Okapi.Tree' round-trip
--   law vocabulary (@printParse@\/@parsePrint@, which compare 'Either'
--   results directly) doesn't apply here. Concrete, executed @>>>@
--   examples stand in for @prop>@ properties on this module. Named
--   @parser@\/@printer@, not @parse@\/@print@, specifically so doctests
--   here (and any caller) never have to fight @Prelude.print@ ambiguity.
type Body :: Type -> Type
data Body a where
    Raw       :: Body (IO LBS.ByteString)
    Json      :: IsoJson a => Body (IO a)
    NoContent :: Body (IO ())
    Form      :: (ToForm a, FromForm a) => Body (IO a)

data ParseError = ParseError deriving (Eq, Show)

type instance Context Body = IO LBS.ByteString
type instance Failure Body = ParseError

formDecode :: FromForm a => LBS.ByteString -> IO a
formDecode bs = case urlDecodeAsForm bs of
    Left err -> fail (show err)
    Right x  -> pure x

parser :: Body a -> IO LBS.ByteString -> Either ParseError a
parser Raw       ioLbs = Right ioLbs
parser Json      ioLbs = Right (ioLbs >>= jsonDecode)
parser Form      ioLbs = Right (ioLbs >>= formDecode)
parser NoContent _     = Right (pure ())

printer :: Body a -> a -> IO LBS.ByteString
printer Raw       ioLbs = ioLbs
printer Json      ioA   = Aeson.encode <$> ioA
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
raw :: Body (IO LBS.ByteString)
raw = Raw

-- | Decode\/encode the body as JSON.
--
-- >>> r3 <- either (error . show) id (parser (json @Int) (pure "42"))
-- >>> r3
-- 42
-- >>> r4 <- printer (json @Int) (pure 42)
-- >>> r4
-- "42"
json :: IsoJson a => Body (IO a)
json = Json

-- | Decode\/encode the body as @application\/x-www-form-urlencoded@.
--
-- >>> r5 <- either (error . show) id (parser (form @Form) (pure "a=1&b=2"))
-- >>> r5
-- fromList [("a","1"),("b","2")]
-- >>> r6 <- printer (form @Form) (pure r5)
-- >>> r6
-- "a=1&b=2"
form :: (ToForm a, FromForm a) => Body (IO a)
form = Form

-- | No body at all — parsing ignores the input, printing produces nothing.
--
-- >>> either (error . show) id (parser noContent (pure "")) :: IO ()
-- >>> r7 <- printer noContent (pure ())
-- >>> r7
-- ""
noContent :: Body (IO ())
noContent = NoContent
