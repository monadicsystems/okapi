{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Request.Path (
    Path (..),
    ParseError (..),
    segment_,
    segment,
    segments,
    raw,
    parse,
    parseExact,
    print,
    LitF (..),
    GPath (..),
    pathCodec,
) where

import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1, Selector (..), (:*:) (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Okapi.Codec (Codec (..), ParseErrorOf, PartOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data
    ( Data (..), Iso (..), Prim (..)
    , boolPrim, charPrim, dayPrim, doublePrim, floatPrim, int16Prim, int32Prim
    , int64Prim, intPrim, integerPrim, localTimePrim, scientificPrim, textPrim
    , timeOfDayPrim, utcTimePrim, uuidPrim
    )
import Prelude hiding (print)

data Path a where
    Seg_ :: Iso Path a -> a -> Path ()
    Seg :: Text -> Iso Path a -> Path a
    Segs :: Iso Path a -> Path (NonEmpty a)
    Raw :: Path [Text]

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Path = [Text]
type instance ParseErrorOf Path = ParseError
type instance PartOf Path = Text

parse :: Codec Path i o -> [Text] -> (Either ParseError o, [Text])
parse = Codec.parser pathAlg
  where
    pathAlg :: forall a. Path a -> [Text] -> (Either ParseError a, [Text])
    pathAlg (Seg_ vIso x) (t : ts)
        | t == vIso.encode x = (Right (), ts)
        | otherwise = (Left ParseError, t : ts)
    pathAlg (Seg_ _ _) [] = (Left ParseError, [])
    pathAlg (Seg _name vIso) (t : ts) = case vIso.decode t of
        Left _ -> (Left ParseError, t : ts)
        Right v -> (Right v, ts)
    pathAlg (Seg _ _) [] = (Left ParseError, [])
    pathAlg (Segs vIso) ts = case NEL.nonEmpty ts of
        Nothing -> (Left ParseError, [])
        Just nel -> case traverse vIso.decode (NEL.toList nel) of
            Left _ -> (Left ParseError, ts)
            Right xs -> case NEL.nonEmpty xs of
                Nothing -> (Left ParseError, [])
                Just nel' -> (Right nel', [])
    pathAlg Raw ts = (Right ts, [])

parseExact :: Codec Path i o -> [Text] -> Either (ParseError, [Text]) o
parseExact pathCodec' path = case parse pathCodec' path of
    (Left e, p) -> Left (e, p)
    (Right a, []) -> Right a
    (Right _, p) -> Left (ParseError, p)

print :: Codec Path i o -> i -> [Text]
print = Codec.printer pathPrinter
  where
    pathPrinter :: forall a. Path a -> a -> [Text]
    pathPrinter (Seg_ vIso x) () = [vIso.encode x]
    pathPrinter (Seg _name vIso) v = [vIso.encode v]
    pathPrinter (Segs vIso) nel = map vIso.encode (NEL.toList nel)
    pathPrinter Raw ts = ts

segment_ :: Iso Path a -> a -> Codec Path b ()
segment_ vIso x = Codec.LMap (const ()) (Lift (Seg_ vIso x))

segment :: Text -> Iso Path a -> Codec Path a a
segment name vIso = Lift (Seg name vIso)

segments :: Iso Path a -> Codec Path (NonEmpty a) (NonEmpty a)
segments vIso = Lift (Segs vIso)

raw :: Codec Path [Text] [Text]
raw = Lift Raw

pathIso :: Prim a -> Iso Path a
pathIso (Prim dec enc nfo) = Iso (either (const (Left ParseError)) Right . dec) enc nfo

instance Data Path Int        where iso = pathIso intPrim
instance Data Path Int16      where iso = pathIso int16Prim
instance Data Path Int32      where iso = pathIso int32Prim
instance Data Path Int64      where iso = pathIso int64Prim
instance Data Path Integer    where iso = pathIso integerPrim
instance Data Path Float      where iso = pathIso floatPrim
instance Data Path Double     where iso = pathIso doublePrim
instance Data Path Scientific where iso = pathIso scientificPrim
instance Data Path Bool       where iso = pathIso boolPrim
instance Data Path Char       where iso = pathIso charPrim
instance Data Path Text       where iso = pathIso textPrim
instance Data Path Day        where iso = pathIso dayPrim
instance Data Path LocalTime  where iso = pathIso localTimePrim
instance Data Path UTCTime    where iso = pathIso utcTimePrim
instance Data Path TimeOfDay  where iso = pathIso timeOfDayPrim
instance Data Path UUID       where iso = pathIso uuidPrim

data LitF (sym :: Symbol) = LitF deriving (Eq, Show)

class GPath (f :: Type -> Type) where
    gPathCodec :: Codec Path (f ()) (f ())

instance (GPath f) => GPath (D1 meta f) where
    gPathCodec = FMap M1 $ LMap unM1 gPathCodec

instance (GPath f) => GPath (C1 meta f) where
    gPathCodec = FMap M1 $ LMap unM1 gPathCodec

instance (GPath f, GPath g) => GPath (f :*: g) where
    gPathCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gPathCodec))
            (LMap (\(_ :*: r) -> r) gPathCodec)

instance {-# OVERLAPPING #-} (KnownSymbol sym) => GPath (S1 meta (Rec0 (LitF sym))) where
    gPathCodec =
        let txt = Text.pack (symbolVal (Proxy @sym))
         in FMap (\() -> M1 (K1 LitF)) $ LMap (const ()) $ Lift (Seg_ (iso @Path @Text) txt)

instance (Selector meta, Data Path a) => GPath (S1 meta (Rec0 a)) where
    gPathCodec =
        let name = Text.pack (selName (undefined :: S1 meta (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Seg name (iso @Path @a))

pathCodec :: forall a. (Generic a, GPath (Rep a)) => Codec Path a a
pathCodec = FMap (to @a) $ LMap (from @a) gPathCodec
