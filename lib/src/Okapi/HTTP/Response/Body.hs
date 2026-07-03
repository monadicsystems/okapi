
module Okapi.HTTP.Response.Body (
    Body (..),
    IsoJson,
    ParseError (..),
    parser,
    printer,
    raw,
    json,
    html,
    noContent,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Lucid (Html, renderBS)
import Okapi.Tree (Failure, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Body (IsoJson, jsonDecode)

type Body :: Type -> Type
data Body a where
    Raw       :: Body (IO LBS.ByteString)
    Json      :: IsoJson a => Body (IO a)
    NoContent :: Body (IO ())
    Html      :: Body (IO (Html ()))

data ParseError = ParseError deriving (Eq, Show)

type instance Context Body = IO LBS.ByteString
type instance Failure Body = ParseError

parser :: Tree Body i o -> Parser Body o
parser = Tree.parser alg
  where
    alg :: Body a -> Parser Body a
    alg Raw       ioLbs = (Right ioLbs,                  pure mempty)
    alg Json      ioLbs = (Right (ioLbs >>= jsonDecode), pure mempty)
    alg Html      _     = (Left ParseError,              pure mempty)
    alg NoContent _     = (Right (pure ()),              pure mempty)

printer :: Tree Body i o -> Printer Body i
printer = Tree.printer alg
  where
    alg :: Body a -> Printer Body a
    alg Raw       ioLbs = ioLbs
    alg Json      ioA   = Aeson.encode <$> ioA
    alg Html      ioH   = renderBS <$> ioH
    alg NoContent _     = pure mempty

raw :: Tree Body (IO LBS.ByteString) (IO LBS.ByteString)
raw = Node Raw

json :: IsoJson a => Tree Body (IO a) (IO a)
json = Node Json

html :: Tree Body (IO (Html ())) (IO (Html ()))
html = Node Html

noContent :: Tree Body (IO ()) (IO ())
noContent = Node NoContent
