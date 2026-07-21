{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Request.Path (
    Path (..),
    Base,
    ParseError (..),
    parser,
    printer,
    parseExact,
    seg_,
    lit,
    seg,
    segs,
    base,
    LitF (..),
    GPath (..),
    derived,
) where

import Data.Bifunctor (first)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
-- In the import of `Data.List.NonEmpty':
--   an item called `(:|)'
--   is exported, but it is a data constructor of
--   `NonEmpty'.
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1, Selector (..), (:*:) (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Okapi.HTTP.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context, Tree (..), text)
import Okapi.HTTP.Tree qualified as Tree
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.HTTP.Tree (printParse, int, integer, (=.))
-- >>> import Data.List.NonEmpty (NonEmpty((:|)))
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- let twoSegs = do
--       n <- fst =. seg "n" int
--       m <- snd =. seg "m" int
--       pure (n, m)
-- :}
--
-- >>> :{
-- data Coords = Coords { coordX :: Int, coordY :: Int } deriving (Generic, Eq, Show)
-- :}

data Path i o where
    Seg_ :: Leaf Path a -> a -> Path i ()
    Seg  :: Text -> Leaf Path a -> Path a a
    Segs :: Leaf Path a -> Path (NonEmpty a) (NonEmpty a)
    Base  :: Path Base Base

data ParseError = ParseError deriving (Eq, Show)

type instance Context  Path = [Text]
type instance Failure  Path = ParseError
type instance Piece  Path = Text

parser :: Tree Path i o -> Parser Path o
parser = Tree.parser alg
  where
    alg :: Path i o -> Parser Path o
    alg (Seg_ vLeaf x) (t : ts)
        | t == vLeaf.encode x = (Right (), ts)
        | otherwise            = (Left ParseError, t : ts)
    alg (Seg_ _ _) [] = (Left ParseError, [])
    alg (Seg _name vLeaf) (t : ts) = case vLeaf.decode t of
        Left _  -> (Left ParseError, t : ts)
        Right v -> (Right v, ts)
    alg (Seg _ _) [] = (Left ParseError, [])
    alg (Segs vLeaf) ts = case NEL.nonEmpty ts of
        Nothing  -> (Left ParseError, [])
        Just nel -> case traverse vLeaf.decode (NEL.toList nel) of
            Left _   -> (Left ParseError, ts)
            Right xs -> case NEL.nonEmpty xs of
                Nothing   -> (Left ParseError, [])
                Just nel' -> (Right nel', [])
    alg Base ts = (Right ts, [])

printer :: Tree Path i o -> Printer Path i
printer = Tree.printer alg
  where
    alg :: Path i o -> Printer Path i
    alg (Seg_ vLeaf x) _  = [vLeaf.encode x]
    alg (Seg _name vLeaf) v = [vLeaf.encode v]
    alg (Segs vLeaf) nel = map vLeaf.encode (NEL.toList nel)
    alg Base ts = ts

-- | Parse a full path, requiring every segment be consumed — 'Left' with
--   the leftover segments if any remain, 'Left' with the underlying error
--   if parsing itself failed.
--
-- >>> parseExact (seg "id" int) ["42"]
-- Right 42
-- >>> parseExact (seg "id" int) ["42", "extra"]
-- Left (Right ["extra"])
-- >>> parseExact (seg "id" int) ["nope"]
-- Left (Left ParseError)
parseExact :: Tree Path i o -> [Text] -> Either (Either ParseError [Text]) o
parseExact = Tree.parseExact parser

-- | Match a fixed literal segment.
--
-- >>> printer (seg_ int 42) ()
-- ["42"]
-- >>> parser (seg_ int 42) ["42"]
-- (Right (),[])
-- >>> parser (seg_ int 42) ["99"]
-- (Left ParseError,["99"])
seg_ :: Leaf Path a -> a -> Tree Path i ()
seg_ vLeaf x = Node (Seg_ vLeaf x)

-- | Match a fixed literal text segment — the common case of 'seg_',
--   specialized to 'Text' so callers don't have to spell out the leaf.
--   @lit "user"@ is exactly @seg_ text "user"@.
--
-- >>> parser (lit "user") ["user"]
-- (Right (),[])
-- >>> printer (lit "user") ()
-- ["user"]
lit :: Text -> Tree Path i ()
lit = seg_ text

-- | Parse and print a single typed path segment.
--
-- >>> printer (seg "id" int) (42 :: Int)
-- ["42"]
-- >>> parser (seg "id" int) ["42"]
-- (Right 42,[])
-- >>> parser (seg "id" int) ["hello"]
-- (Left ParseError,["hello"])
-- >>> parser (seg "id" int) []
-- (Left ParseError,[])
--
-- prop> printParse parser printer (seg "n" int) (x :: Int)
-- prop> printParse parser printer (seg "n" integer) x
seg :: Text -> Leaf Path a -> Tree Path a a
seg name vLeaf = Node (Seg name vLeaf)

-- | Parse and print all remaining path segments as a non-empty list.
--
-- prop> printParse parser printer (segs int) (x :| xs)
segs :: Leaf Path a -> Tree Path (NonEmpty a) (NonEmpty a)
segs vLeaf = Node (Segs vLeaf)

-- | Pass all remaining path segments straight through, unconstrained.
--
-- >>> parser base ["a", "b", "c"]
-- (Right ["a","b","c"],[])
-- >>> printer base ["a", "b", "c"]
-- ["a","b","c"]
base :: Tree Path [Text] [Text]
base = Node Base

-- | What 'base' decodes\/encodes to — the maximally unconstrained path slot.
type Base = [Text]

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

-- | Generically derive a 'Path' codec from a record's field names and
--   'HasLeaf' instances.
--
-- >>> parser (derived @Coords) ["3", "4"]
-- (Right (Coords {coordX = 3, coordY = 4}),[])
-- >>> printer (derived @Coords) (Coords 3 4)
-- ["3","4"]
derived :: forall a. (Generic a, GPath (Rep a)) => Tree Path a a
derived = FMap (to @a) $ LMap (from @a) gPathCodec

-- $combined
-- >>> parser twoSegs ["42", "99", "extra"]
-- (Right (42,99),["extra"])
--
-- prop> printParse parser printer twoSegs (xy :: (Int, Int))
