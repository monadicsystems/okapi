{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | A sub-DSL for an OpenAPI @deepObject@-encoded query parameter: an object
--   whose fields render as @name[field]=value@ (one level). Separator-agnostic;
--   the bracket prefix (@name[…]@) is applied by the @deepObject@ combinator in
--   "Okapi.Protocol.Request.Query". Field values use 'IsoDeepObjectData'.
module Okapi.Protocol.Request.Query.DeepObject (
    DeepObject (..),
    Fields,
    ParseError (..),
    ToDeepObjectData (..),
    FromDeepObjectData (..),
    IsoDeepObjectData,
    parse,
    print,
    field,
    field',
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List (partition)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (FromQueryData, ToQueryData, parseQueryParam, toQueryParam)
import Prelude hiding (print)

-- | Render a deepObject field value. Defaults to the query encoding
--   (deepObject values are percent-encoded query values), overridable.
class ToDeepObjectData a where
    toDeepObjectData :: a -> Text
    default toDeepObjectData :: ToQueryData a => a -> Text
    toDeepObjectData = toQueryParam

-- | Parse a deepObject field value. Defaults to the query decoding.
class FromDeepObjectData a where
    fromDeepObjectData :: Text -> Either Text a
    default fromDeepObjectData :: FromQueryData a => Text -> Either Text a
    fromDeepObjectData = parseQueryParam

type IsoDeepObjectData a = (ToDeepObjectData a, FromDeepObjectData a)

instance ToDeepObjectData Text
instance FromDeepObjectData Text
instance ToDeepObjectData Int
instance FromDeepObjectData Int
instance ToDeepObjectData Bool
instance FromDeepObjectData Bool

-- | Inner fields (key without the @name[…]@ bracket), keyed by field name.
type Fields = [(ByteString, Maybe ByteString)]

type DeepObject :: Type -> Type
data DeepObject a where
    Field  :: IsoDeepObjectData a => Text -> DeepObject a
    Field' :: IsoDeepObjectData a => Text -> DeepObject (Maybe a)

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf      DeepObject = Fields
type instance ParseErrorOf DeepObject = ParseError

parse :: Codec DeepObject i o -> Fields -> (Either ParseError o, Fields)
parse = Codec.parser alg
  where
    alg :: forall a. DeepObject a -> Fields -> (Either ParseError a, Fields)
    alg (Field key) fs =
        case partition (\(k, _) -> k == encodeUtf8 key) fs of
            ([], _)                  -> (Left ParseError, fs)
            ((_, Nothing) : _, _)    -> (Left ParseError, fs)
            ((_, Just v) : _, rest)  -> case fromDeepObjectData (decodeUtf8Lenient v) of
                Left _  -> (Left ParseError, fs)
                Right x -> (Right x, rest)
    alg (Field' key) fs =
        case partition (\(k, _) -> k == encodeUtf8 key) fs of
            ([], _)                   -> (Right Nothing, fs)
            ((_, Nothing) : _, rest)  -> (Right Nothing, rest)
            ((_, Just v) : _, rest)   -> case fromDeepObjectData (decodeUtf8Lenient v) of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)

print :: Codec DeepObject i o -> i -> Fields
print = Codec.printer pr
  where
    pr :: forall a. DeepObject a -> a -> Fields
    pr (Field key) x         = [(encodeUtf8 key, Just (encodeUtf8 (toDeepObjectData x)))]
    pr (Field' _) Nothing    = []
    pr (Field' key) (Just x) = [(encodeUtf8 key, Just (encodeUtf8 (toDeepObjectData x)))]

-- | Required object field; serialized as @name[key]=value@ by the @deepObject@ op.
field :: IsoDeepObjectData a => Text -> Codec DeepObject a a
field key = Lift (Field key)

-- | Optional object field; yields 'Nothing' when absent.
field' :: IsoDeepObjectData a => Text -> Codec DeepObject (Maybe a) (Maybe a)
field' key = Lift (Field' key)
