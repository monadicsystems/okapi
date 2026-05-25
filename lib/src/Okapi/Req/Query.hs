{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Query (
    Query (..),
    ParseError (..),
    parse,
    print,
    raw,
    param,
    flag,
    optional,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Web.HttpApiData qualified as HTTP.Api
import Prelude hiding (print)

type IsoHttpApiData a = (HTTP.Api.FromHttpApiData a, HTTP.Api.ToHttpApiData a)

type Query :: Type -> Type
data Query a where
    Raw :: Query HTTP.Query
    Param :: IsoHttpApiData a => Text -> Query a
    Flag :: Text -> Query Bool
    Optional :: Codec Query a a -> Query (Maybe a)

data ParseError = ParseError

type instance StateOf Query = HTTP.Query
type instance ParseErrorOf Query = ParseError

parse :: Codec Query i o -> HTTP.Query -> (Either ParseError o, HTTP.Query)
parse = Codec.parser queryAlg
  where
    queryAlg = undefined

print :: Codec Query i o -> i -> HTTP.Query
print = Codec.printer queryPrinter
  where
    queryPrinter = undefined

raw :: Codec Query HTTP.Query HTTP.Query
raw = Embed Raw

param :: IsoHttpApiData a => Text -> Codec Query a a
param key = Embed (Param key)

flag :: Text -> Codec Query Bool Bool
flag key = Embed (Flag key)

optional :: Codec Query a a -> Codec Query (Maybe a) (Maybe a)
optional c = Embed (Optional c)
