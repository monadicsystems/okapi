{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Req.Path (
    Path (..),
    ParseError (..),
    lit,
    seg,
    segs,
    raw,
    parse,
    parseExact,
    print,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (FromPathData (..), IsoPathData, ToPathData (..))
import Prelude hiding (print)

data Path a where
    Lit  :: IsoPathData a => a -> Path ()
    Seg  :: IsoPathData a => Text -> Path a
    Segs :: IsoPathData a => Path (NonEmpty a)
    Raw  :: Path [Text]

data ParseError = ParseError

type instance StateOf Path = [Text]
type instance ParseErrorOf Path = ParseError

parse :: Codec Path i o -> [Text] -> (Either ParseError o, [Text])
parse = Codec.parser pathAlg
  where
    pathAlg :: forall a. Path a -> [Text] -> (Either ParseError a, [Text])
    pathAlg (Lit x) (t : ts)
        | t == toUrlPiece x = (Right (), ts)
        | otherwise         = (Left ParseError, t : ts)
    pathAlg (Lit _) [] = (Left ParseError, [])
    pathAlg (Seg _name) (t : ts) = case parseUrlPiece t of
        Left _  -> (Left ParseError, t : ts)
        Right v -> (Right v, ts)
    pathAlg (Seg _name) [] = (Left ParseError, [])
    pathAlg Segs ts = case NEL.nonEmpty ts of
        Nothing  -> (Left ParseError, [])
        Just nel -> case traverse parseUrlPiece (NEL.toList nel) of
            Left _   -> (Left ParseError, ts)
            Right xs -> case NEL.nonEmpty xs of
                Nothing   -> (Left ParseError, [])
                Just nel' -> (Right nel', [])
    pathAlg Raw ts = (Right ts, [])

parseExact :: Codec Path i o -> [Text] -> Either (ParseError, [Text]) o
parseExact pathCodec path = case parse pathCodec path of
    (Left e, p)   -> Left (e, p)
    (Right a, []) -> Right a
    (Right _, p)  -> Left (ParseError, p)

print :: Codec Path i o -> i -> [Text]
print = Codec.printer pathPrinter
  where
    pathPrinter :: forall a. Path a -> a -> [Text]
    pathPrinter (Lit x)     ()  = [toUrlPiece x]
    pathPrinter (Seg _name) v   = [toUrlPiece v]
    pathPrinter Segs        nel = map toUrlPiece (NEL.toList nel)
    pathPrinter Raw         ts  = ts

lit :: IsoPathData a => a -> Codec Path b ()
lit x = Codec.LMap (const ()) (Embed (Lit x))

seg :: IsoPathData a => Text -> Codec Path a a
seg name = Embed (Seg name)

segs :: IsoPathData a => Codec Path (NonEmpty a) (NonEmpty a)
segs = Embed Segs

raw :: Codec Path [Text] [Text]
raw = Embed Raw
