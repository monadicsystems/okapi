{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Request.Path (
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

import Data.Bifunctor (first)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1, Selector (..), (:*:) (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Okapi.Leaf (ErrorOf, HasLeaf (..), Info (..), Leaf (..), PieceOf, StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Prelude hiding (print)
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

data Path a where
    Seg_ :: Leaf Path a -> a -> Path ()
    Seg  :: Text -> Leaf Path a -> Path a
    Segs :: Leaf Path a -> Path (NonEmpty a)
    Raw  :: Path [Text]

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf  Path = [Text]
type instance ErrorOf  Path = ParseError
type instance PieceOf  Path = Text

parse :: Tree Path i o -> [Text] -> (Either ParseError o, [Text])
parse = Tree.grow pathAlg
  where
    pathAlg :: forall a. Path a -> [Text] -> (Either ParseError a, [Text])
    pathAlg (Seg_ vLeaf x) (t : ts)
        | t == vLeaf.encode x = (Right (), ts)
        | otherwise = (Left ParseError, t : ts)
    pathAlg (Seg_ _ _) [] = (Left ParseError, [])
    pathAlg (Seg _name vLeaf) (t : ts) = case vLeaf.decode t of
        Left _  -> (Left ParseError, t : ts)
        Right v -> (Right v, ts)
    pathAlg (Seg _ _) [] = (Left ParseError, [])
    pathAlg (Segs vLeaf) ts = case NEL.nonEmpty ts of
        Nothing  -> (Left ParseError, [])
        Just nel -> case traverse vLeaf.decode (NEL.toList nel) of
            Left _   -> (Left ParseError, ts)
            Right xs -> case NEL.nonEmpty xs of
                Nothing   -> (Left ParseError, [])
                Just nel' -> (Right nel', [])
    pathAlg Raw ts = (Right ts, [])

parseExact :: Tree Path i o -> [Text] -> Either (ParseError, [Text]) o
parseExact pathCodec' path = case parse pathCodec' path of
    (Left e, p)  -> Left (e, p)
    (Right a, []) -> Right a
    (Right _, p) -> Left (ParseError, p)

print :: Tree Path i o -> i -> [Text]
print = Tree.eat pathPrinter
  where
    pathPrinter :: forall a. Path a -> a -> [Text]
    pathPrinter (Seg_ vLeaf x) () = [vLeaf.encode x]
    pathPrinter (Seg _name vLeaf) v = [vLeaf.encode v]
    pathPrinter (Segs vLeaf) nel = map vLeaf.encode (NEL.toList nel)
    pathPrinter Raw ts = ts

segment_ :: Leaf Path a -> a -> Tree Path () ()
segment_ vLeaf x = LMap (const ()) (Node (Seg_ vLeaf x))

segment :: Text -> Leaf Path a -> Tree Path a a
segment name vLeaf = Node (Seg name vLeaf)

segments :: Leaf Path a -> Tree Path (NonEmpty a) (NonEmpty a)
segments vLeaf = Node (Segs vLeaf)

raw :: Tree Path [Text] [Text]
raw = Node Raw

instance HasLeaf Path Int     where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "integer" Nothing)
instance HasLeaf Path Int16   where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "integer" (Just "int32"))
instance HasLeaf Path Int32   where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "integer" (Just "int32"))
instance HasLeaf Path Int64   where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "integer" (Just "int64"))
instance HasLeaf Path Integer where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "integer" Nothing)
instance HasLeaf Path Text    where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "string" Nothing)
instance HasLeaf Path UUID    where leaf = Leaf (first (const ParseError) . parseUrlPiece) toUrlPiece (Info "string" (Just "uuid"))

data LitF (sym :: Symbol) = LitF deriving (Eq, Show)

class GPath (f :: Type -> Type) where
    gPathCodec :: Tree Path (f ()) (f ())

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
         in FMap (\() -> M1 (K1 LitF)) $ LMap (const ()) $ Node (Seg_ (leaf @Path @Text) txt)

instance (Selector meta, HasLeaf Path a) => GPath (S1 meta (Rec0 a)) where
    gPathCodec =
        let name = Text.pack (selName (undefined :: S1 meta (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Seg name (leaf @Path @a))

pathCodec :: forall a. (Generic a, GPath (Rep a)) => Tree Path a a
pathCodec = FMap (to @a) $ LMap (from @a) gPathCodec