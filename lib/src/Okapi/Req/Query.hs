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
    paramOpt,
    flag,
    flagOpt,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types qualified as HTTP
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (IsoQueryData)
import Prelude hiding (print)

type Query :: Type -> Type
data Query a where
    Raw      :: Query HTTP.Query
    Param    :: IsoQueryData a => Text -> Query a
    ParamOpt :: IsoQueryData a => Text -> Query (Maybe a)
    Flag     :: Text -> Query ()
    FlagOpt  :: Text -> Query Bool

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

param :: IsoQueryData a => Text -> Codec Query a a
param key = Embed (Param key)

paramOpt :: IsoQueryData a => Text -> Codec Query (Maybe a) (Maybe a)
paramOpt key = Embed (ParamOpt key)

flag :: Text -> Codec Query () ()
flag key = Embed (Flag key)

flagOpt :: Text -> Codec Query Bool Bool
flagOpt key = Embed (FlagOpt key)
