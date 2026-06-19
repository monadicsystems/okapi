{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Protocol.Request.Path (
    Path (..),
    ParseError (..),
    seg_,
    seg,
    segs,
    raw,
    parse,
    parseExact,
    print,
    LitF (..),
    GPath (..),
    pathCodec,
) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (D1, C1, S1, K1 (..), M1 (..), Rec0, (:*:) (..), Generic (..), Selector (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Okapi.Codec (Codec (..), ParseErrorOf, StateOf)
import Okapi.Codec qualified as Codec
import Okapi.Data (FromPathData (..), IsoPathData, ToPathData (..))
import Prelude hiding (print)

data Path a where
    Seg_ :: IsoPathData a => a -> Path ()
    Seg  :: IsoPathData a => Text -> Path a
    Segs :: IsoPathData a => Path (NonEmpty a)
    Raw  :: Path [Text]

data ParseError = ParseError deriving (Eq, Show)

type instance StateOf Path = [Text]
type instance ParseErrorOf Path = ParseError

parse :: Codec Path i o -> [Text] -> (Either ParseError o, [Text])
parse = Codec.parser pathAlg
  where
    pathAlg :: forall a. Path a -> [Text] -> (Either ParseError a, [Text])
    pathAlg (Seg_ x) (t : ts)
        | t == toUrlPiece x = (Right (), ts)
        | otherwise         = (Left ParseError, t : ts)
    pathAlg (Seg_ _) [] = (Left ParseError, [])
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
parseExact pathCodec' path = case parse pathCodec' path of
    (Left e, p)   -> Left (e, p)
    (Right a, []) -> Right a
    (Right _, p)  -> Left (ParseError, p)

print :: Codec Path i o -> i -> [Text]
print = Codec.printer pathPrinter
  where
    pathPrinter :: forall a. Path a -> a -> [Text]
    pathPrinter (Seg_ x)    ()  = [toUrlPiece x]
    pathPrinter (Seg _name) v   = [toUrlPiece v]
    pathPrinter Segs        nel = map toUrlPiece (NEL.toList nel)
    pathPrinter Raw         ts  = ts

-- | Match a literal path segment; contributes nothing to the decoded value.
seg_ :: IsoPathData a => a -> Codec Path b ()
seg_ x = Codec.LMap (const ()) (Lift (Seg_ x))

-- | Match and capture a single typed path segment, identified by a name for documentation.
seg :: IsoPathData a => Text -> Codec Path a a
seg name = Lift (Seg name)

-- | Match and capture all remaining path segments as a non-empty list.
segs :: IsoPathData a => Codec Path (NonEmpty a) (NonEmpty a)
segs = Lift Segs

raw :: Codec Path [Text] [Text]
raw = Lift Raw


-- ── Generic path deriving ─────────────────────────────────────────────────────

-- | Record field type for a literal path segment. The @sym@ Symbol is the
--   segment text to match; the field name in the record is ignored.
data LitF (sym :: Symbol) = LitF deriving (Eq, Show)

class GPath (f :: Type -> Type) where
    gPathCodec :: Codec Path (f ()) (f ())

instance GPath f => GPath (D1 meta f) where
    gPathCodec = FMap M1 $ LMap unM1 gPathCodec

instance GPath f => GPath (C1 meta f) where
    gPathCodec = FMap M1 $ LMap unM1 gPathCodec

instance (GPath f, GPath g) => GPath (f :*: g) where
    gPathCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gPathCodec))
            (LMap (\(_ :*: r) -> r) gPathCodec)

instance {-# OVERLAPPING #-} KnownSymbol sym => GPath (S1 meta (Rec0 (LitF sym))) where
    gPathCodec =
        let txt = Text.pack (symbolVal (Proxy @sym))
        in FMap (\() -> M1 (K1 LitF)) $ LMap (const ()) $ Lift (Seg_ txt)

instance (Selector meta, IsoPathData a) => GPath (S1 meta (Rec0 a)) where
    gPathCodec =
        let name = Text.pack (selName (undefined :: S1 meta (Rec0 a) ()))
        in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Lift (Seg name)

-- | Build a 'Path' codec from a Generic record type.
--   @LitF sym@ fields match literal segments; other @IsoPathData@ fields capture typed segments.
--   Field names are used as the segment label for @Seg@.
pathCodec :: forall a. (Generic a, GPath (Rep a)) => Codec Path a a
pathCodec = FMap (to @a) $ LMap (from @a) gPathCodec
