
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
import Okapi.Tree (Failure, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Body (IsoJson, jsonDecode)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

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

parser :: Tree Body i o -> Parser Body o
parser = Tree.parser alg
  where
    alg :: Body a -> Parser Body a
    alg Raw       ioLbs = (Right ioLbs,                  pure mempty)
    alg Json      ioLbs = (Right (ioLbs >>= jsonDecode), pure mempty)
    alg Form      ioLbs = (Right (ioLbs >>= formDecode), pure mempty)
    alg NoContent _     = (Right (pure ()),              pure mempty)

printer :: Tree Body i o -> Printer Body i
printer = Tree.printer alg
  where
    alg :: Body a -> Printer Body a
    alg Raw       ioLbs = ioLbs
    alg Json      ioA   = Aeson.encode <$> ioA
    alg Form      ioA   = urlEncodeAsForm <$> ioA
    alg NoContent _     = pure mempty

raw :: Tree Body (IO LBS.ByteString) (IO LBS.ByteString)
raw = Node Raw

json :: IsoJson a => Tree Body (IO a) (IO a)
json = Node Json

form :: (ToForm a, FromForm a) => Tree Body (IO a) (IO a)
form = Node Form

noContent :: Tree Body (IO ()) (IO ())
noContent = Node NoContent
