{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Request.Path (
    Path (..),
    ParseError (..),
    parser,
    printer,
    parseExact,
    segment_,
    segment,
    segments,
    raw,
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
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context, Tree (..), widen)
import Okapi.Tree qualified as Tree
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.Tree (printParse, int, integer, (=.))
-- >>> import Data.List.NonEmpty (NonEmpty((:|)))
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- let twoSegs = do
--       n <- fst =. segment "n" int
--       m <- snd =. segment "m" int
--       pure (n, m)
-- :}
--
-- >>> :{
-- data Coords = Coords { coordX :: Int, coordY :: Int } deriving (Generic, Eq, Show)
-- :}

data Path a where
    Seg_ :: Leaf Path a -> a -> Path ()
    Seg  :: Text -> Leaf Path a -> Path a
    Segs :: Leaf Path a -> Path (NonEmpty a)
    Raw  :: Path [Text]

data ParseError = ParseError deriving (Eq, Show)

type instance Context  Path = [Text]
type instance Failure  Path = ParseError
type instance Piece  Path = Text

parser :: Tree Path i o -> Parser Path o
parser = Tree.parser alg
  where
    alg :: Path a -> Parser Path a
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
    alg Raw ts = (Right ts, [])

printer :: Tree Path i o -> Printer Path i
printer = Tree.printer alg
  where
    alg :: Path a -> Printer Path a
    alg (Seg_ vLeaf x) () = [vLeaf.encode x]
    alg (Seg _name vLeaf) v = [vLeaf.encode v]
    alg (Segs vLeaf) nel = map vLeaf.encode (NEL.toList nel)
    alg Raw ts = ts

-- | Parse a full path, requiring every segment be consumed — 'Left' with
--   the leftover segments if any remain, 'Left' with the underlying error
--   if parsing itself failed.
--
-- >>> parseExact (segment "id" int) ["42"]
-- Right 42
-- >>> parseExact (segment "id" int) ["42", "extra"]
-- Left (Right ["extra"])
-- >>> parseExact (segment "id" int) ["nope"]
-- Left (Left ParseError)
parseExact :: Tree Path i o -> [Text] -> Either (Either ParseError [Text]) o
parseExact = Tree.parseExact parser

-- | Match a fixed literal segment.
--
-- >>> printer (segment_ int 42) ()
-- ["42"]
-- >>> parser (segment_ int 42) ["42"]
-- (Right (),[])
-- >>> parser (segment_ int 42) ["99"]
-- (Left ParseError,["99"])
segment_ :: Leaf Path a -> a -> Tree Path i ()
segment_ vLeaf x = widen (Node (Seg_ vLeaf x))

-- | Parse and print a single typed path segment.
--
-- >>> printer (segment "id" int) (42 :: Int)
-- ["42"]
-- >>> parser (segment "id" int) ["42"]
-- (Right 42,[])
-- >>> parser (segment "id" int) ["hello"]
-- (Left ParseError,["hello"])
-- >>> parser (segment "id" int) []
-- (Left ParseError,[])
--
-- prop> printParse parser printer (segment "n" int) (x :: Int)
-- prop> printParse parser printer (segment "n" integer) x
segment :: Text -> Leaf Path a -> Tree Path a a
segment name vLeaf = Node (Seg name vLeaf)

-- | Parse and print all remaining path segments as a non-empty list.
--
-- prop> printParse parser printer (segments int) (x :| xs)
segments :: Leaf Path a -> Tree Path (NonEmpty a) (NonEmpty a)
segments vLeaf = Node (Segs vLeaf)

-- | Pass all remaining path segments straight through, unconstrained.
--
-- >>> parser raw ["a", "b", "c"]
-- (Right ["a","b","c"],[])
-- >>> printer raw ["a", "b", "c"]
-- ["a","b","c"]
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
