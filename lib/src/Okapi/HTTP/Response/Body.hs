{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Response.Body (
    Body (..),
    IsoJson,
    ParseError (..),
    parse,
    print,
    raw,
    json,
    html,
    noContent,
) where

import Control.Applicative (liftA2)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Lucid (Html, renderBS)
import Okapi.Leaf (ErrorOf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Prelude hiding (print)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

type Body :: Type -> Type
data Body a where
    Raw       :: Body (IO LBS.ByteString)
    Json      :: IsoJson a => Body (IO a)
    NoContent :: Body (IO ())
    Html      :: Body (IO (Html ()))

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Body = IO LBS.ByteString
type instance ErrorOf Body = ParseError

parse :: Tree Body i o -> IO LBS.ByteString -> (Either ParseError o, IO LBS.ByteString)
parse = Tree.grow bodyAlg
  where
    bodyAlg :: forall a. Body a -> IO LBS.ByteString -> (Either ParseError a, IO LBS.ByteString)
    bodyAlg Raw       ioLbs = (Right ioLbs,                  pure mempty)
    bodyAlg Json      ioLbs = (Right (ioLbs >>= jsonDecode), pure mempty)
    bodyAlg Html      _     = (Left ParseError,              pure mempty)
    bodyAlg NoContent _     = (Right (pure ()),              pure mempty)

jsonDecode :: IsoJson a => LBS.ByteString -> IO a
jsonDecode bs = case Aeson.eitherDecode bs of
    Left err -> fail err
    Right x  -> pure x

print :: Tree Body i o -> i -> IO LBS.ByteString
print = go
  where
    go :: forall i' o'. Tree Body i' o' -> i' -> IO LBS.ByteString
    go (Pure _)         _     = pure mempty
    go (FMap _ c)       i     = go c i
    go (LMap f c)       i     = go c (f i)
    go (Apply cf cx)    i     = liftA2 (<>) (go cf i) (go cx i)
    go (Node Raw)       ioLbs = ioLbs
    go (Node Json)      ioA   = Aeson.encode <$> ioA
    go (Node Html)      ioH   = renderBS <$> ioH
    go (Node NoContent) _     = pure mempty

raw :: Tree Body (IO LBS.ByteString) (IO LBS.ByteString)
raw = Node Raw

json :: IsoJson a => Tree Body (IO a) (IO a)
json = Node Json

html :: Tree Body (IO (Html ())) (IO (Html ()))
html = Node Html

noContent :: Tree Body (IO ()) (IO ())
noContent = Node NoContent
