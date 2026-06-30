{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Request.Body (
    Body (..),
    IsoJson,
    ParseError (..),
    parse,
    print,
    raw,
    json,
    form,
    noContent,
) where

import Control.Applicative (liftA2)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Okapi.Leaf (ErrorOf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)
import Prelude hiding (print)

type IsoJson a = (Aeson.FromJSON a, Aeson.ToJSON a, ToSchema a)

type Body :: Type -> Type
data Body a where
    Raw       :: Body (IO LBS.ByteString)
    Json      :: IsoJson a => Body (IO a)
    NoContent :: Body (IO ())
    Form      :: (ToForm a, FromForm a) => Body (IO a)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Body = IO LBS.ByteString
type instance ErrorOf Body = ParseError

parse :: Tree Body i o -> IO LBS.ByteString -> (Either ParseError o, IO LBS.ByteString)
parse = Tree.grow bodyAlg
  where
    bodyAlg :: forall a. Body a -> IO LBS.ByteString -> (Either ParseError a, IO LBS.ByteString)
    bodyAlg Raw       ioLbs = (Right ioLbs,                  pure mempty)
    bodyAlg Json      ioLbs = (Right (ioLbs >>= jsonDecode), pure mempty)
    bodyAlg Form      ioLbs = (Right (ioLbs >>= formDecode), pure mempty)
    bodyAlg NoContent _     = (Right (pure ()),              pure mempty)

jsonDecode :: IsoJson a => LBS.ByteString -> IO a
jsonDecode bs = case Aeson.eitherDecode bs of
    Left err -> fail err
    Right x  -> pure x

formDecode :: FromForm a => LBS.ByteString -> IO a
formDecode bs = case urlDecodeAsForm bs of
    Left err -> fail (show err)
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
    go (Node Form)      ioA   = urlEncodeAsForm <$> ioA
    go (Node NoContent) _     = pure mempty

raw :: Tree Body (IO LBS.ByteString) (IO LBS.ByteString)
raw = Node Raw

json :: IsoJson a => Tree Body (IO a) (IO a)
json = Node Json

form :: (ToForm a, FromForm a) => Tree Body (IO a) (IO a)
form = Node Form

noContent :: Tree Body (IO ()) (IO ())
noContent = Node NoContent
