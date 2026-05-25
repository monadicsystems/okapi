{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Path (
    Path (..),
    ParseError (..),
    lit,
    param,
    blob,
    raw,
    parse,
    parseExact,
    print,
) where

import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Web.HttpApiData qualified as HTTP
import Prelude hiding (print)

type IsoHttpApiData a = (HTTP.FromHttpApiData a, HTTP.ToHttpApiData a)

data Path a where
    Lit :: Text -> Path ()
    Param :: (Typeable a, IsoHttpApiData a) => Path a
    Blob :: (Typeable a, IsoHttpApiData a) => Path (NonEmpty a)
    Raw :: Path [Text]

data ParseError = ParseError

type instance StateOf Path = [Text]
type instance ParseErrorOf Path = ParseError

parse :: Codec Path i o -> [Text] -> (Either ParseError o, [Text])
parse = Codec.parser pathAlg
  where
    pathAlg :: forall a. Path a -> [Text] -> (Either ParseError a, [Text])
    pathAlg (Lit txt) (t : ts)
        | t == txt = (Right (), ts)
        | otherwise = (Left ParseError, t : ts)
    pathAlg (Lit _) [] = (Left ParseError, [])
    pathAlg Param (t : ts) = case HTTP.parseUrlPiece t of
        Left _ -> (Left ParseError, t : ts)
        Right x -> (Right x, ts)
    pathAlg Param [] = (Left ParseError, [])
    pathAlg Blob ts = case NEL.nonEmpty ts of
        Nothing -> (Left ParseError, [])
        Just nel -> case traverse HTTP.parseUrlPiece (NEL.toList nel) of
            Left _ -> (Left ParseError, ts)
            Right xs -> case NEL.nonEmpty xs of
                Nothing -> (Left ParseError, [])
                Just nel' -> (Right nel', [])
    pathAlg Raw ts = (Right ts, [])

parseExact :: Codec Path i o -> [Text] -> Either (ParseError, [Text]) o
parseExact pathCodec path = case parse pathCodec path of
    (Left e, p) -> Left (e, p)
    (Right a, []) -> Right a
    (Right _, p) -> Left (ParseError, p)

print :: Codec Path i o -> i -> [Text]
print = Codec.printer pathPrinter
  where
    pathPrinter :: forall a. Path a -> a -> [Text]
    pathPrinter (Lit txt) () = [txt]
    pathPrinter Param x = [HTTP.toUrlPiece x]
    pathPrinter Blob nel = map HTTP.toUrlPiece (NEL.toList nel)
    pathPrinter Raw ts = ts

lit :: Text -> Codec Path a ()
lit t = lmap (const ()) (Embed (Lit t))
  where
    lmap f c = Codec.LMap f c

param :: (Typeable a, IsoHttpApiData a) => Codec Path a a
param = Embed Param

blob :: (Typeable a, IsoHttpApiData a) => Codec Path (NonEmpty a) (NonEmpty a)
blob = Embed Blob

raw :: Codec Path [Text] [Text]
raw = Embed Raw
