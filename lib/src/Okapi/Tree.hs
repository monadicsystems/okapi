
module Okapi.Tree (
    Context,
    Failure,
    Piece,
    Parser,
    Printer,
    Info (..),
    Leaf (..),
    HasLeaf (..),
    int,
    int16,
    int32,
    int64,
    integer,
    bool,
    float,
    double,
    scientific,
    text,
    day,
    localTime,
    utcTime,
    timeOfDay,
    uuid,
    Tree (..),
    SymTree,
    Tag (..),
    annotate,
    (=.),
    cost,
    parser,
    printer,
    purify,
    parseWith,
    printWith,
    printParseWith,
    printParse,
    parsePrint,
    parsePrintOr,
    printParseWeak,
    parsePrintWeak,
    parseExact,
    leafPrintParse,
    leafParsePrint,
    leafParsePrintOr,
    leafPrintParsePrint,
    leafParsePrintParse,
    leafParsePrintParseOr,
) where

import Data.Aeson (Value)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Profunctor
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)

-- ── Leaf ─────────────────────────────────────────────────────────────────────

type Context :: (Type -> Type) -> Type
type family Context t

type Failure :: (Type -> Type) -> Type
type family Failure t

type Piece :: (Type -> Type) -> Type
type family Piece t

type Parser t a = Context t -> (Either (Failure t) a, Context t)
type Printer t a = a -> Context t

data Info = Info
    { typeName :: Text
    , format   :: Maybe Text
    }
    deriving (Eq, Show)

type Leaf :: (Type -> Type) -> Type -> Type
data Leaf t a = Leaf
    { decode :: Piece t -> Either (Failure t) a
    , encode :: a -> Piece t
    , info   :: Info
    }

class HasLeaf t a where
    leaf :: Leaf t a

int :: (HasLeaf t Int) => Leaf t Int
int = leaf

int16 :: (HasLeaf t Int16) => Leaf t Int16
int16 = leaf

int32 :: (HasLeaf t Int32) => Leaf t Int32
int32 = leaf

int64 :: (HasLeaf t Int64) => Leaf t Int64
int64 = leaf

integer :: (HasLeaf t Integer) => Leaf t Integer
integer = leaf

bool :: (HasLeaf t Bool) => Leaf t Bool
bool = leaf

float :: (HasLeaf t Float) => Leaf t Float
float = leaf

double :: (HasLeaf t Double) => Leaf t Double
double = leaf

scientific :: (HasLeaf t Scientific) => Leaf t Scientific
scientific = leaf

text :: (HasLeaf t Text) => Leaf t Text
text = leaf

day :: (HasLeaf t Day) => Leaf t Day
day = leaf

localTime :: (HasLeaf t LocalTime) => Leaf t LocalTime
localTime = leaf

utcTime :: (HasLeaf t UTCTime) => Leaf t UTCTime
utcTime = leaf

timeOfDay :: (HasLeaf t TimeOfDay) => Leaf t TimeOfDay
timeOfDay = leaf

uuid :: (HasLeaf t UUID) => Leaf t UUID
uuid = leaf

-- ── Tree ─────────────────────────────────────────────────────────────────────

type Tree :: (Type -> Type) -> Type -> Type -> Type
data Tree t i o where
    FMap     :: (o -> o') -> Tree t i o -> Tree t i o'
    LMap     :: (i -> i') -> Tree t i' o -> Tree t i o
    Pure     :: o -> Tree t i o
    Apply    :: Tree t i (o -> o') -> Tree t i o -> Tree t i o'
    Node     :: t a -> Tree t a a
    Annotate :: [Tag] -> Tree t i o -> Tree t i o

-- | A symmetrical @Tree t i o@ where @i ~ o@.
type SymTree t a = Tree t a a

-- | Metadata attached to a 'Tree' node that isn't part of its parse\/print
--   behavior — e.g. an OpenAPI description or example on a single path
--   segment or query parameter. A list, not a record: duplicates are
--   allowed and merge order is left to whatever consumes them (last tag
--   wins for singular fields, by convention — see
--   'Okapi.Artifact.OpenApi'). 'Group' and 'Extension' are meaningful only
--   at the whole-contract level ('Okapi.Mode.Forest.annotate'), not here —
--   see there.
data Tag
    = Description Text
    | Example Value
    | Deprecated
    | Group Text
    | Extension Text Value
    deriving (Eq, Show)

-- | Attach metadata to any 'Tree' node — never changes parse\/print
--   behavior, purely a documentation\/introspection layer (consumed by
--   'Okapi.Artifact.OpenApi'). Confirmed transparent against a minimal toy
--   instance (a single-context-cell integer leaf) — 'annotate'-wrapping a
--   node changes neither direction of the round trip:
--
-- >>> data ToyT a where TRaw :: ToyT Int
-- >>> type instance Context ToyT = [Int]
-- >>> type instance Failure ToyT = ()
-- >>> let toyParser = parser (\TRaw ctx -> case ctx of { (x : xs) -> (Right x, xs); [] -> (Left (), []) })
-- >>> let toyPrinter = printer (\TRaw i -> [i])
-- >>> let toyTree = Node TRaw :: Tree ToyT Int Int
-- >>> printParse toyParser toyPrinter (annotate [Deprecated] toyTree) 5 == printParse toyParser toyPrinter toyTree 5
-- True
-- >>> parsePrint toyParser toyPrinter (annotate [Deprecated] toyTree) [5] == parsePrint toyParser toyPrinter toyTree [5]
-- True
--
-- (There's also 'Okapi.Mode.Forest.annotate', a separate function for
-- annotating a whole @req :-> res@ contract rather than one 'Tree' node —
-- qualify the import if both are needed in the same file.)
annotate :: [Tag] -> Tree t i o -> Tree t i o
annotate = Annotate

instance Functor (Tree t i) where
    fmap = FMap

instance Applicative (Tree t i) where
    pure = Pure
    (<*>) = Apply

instance Profunctor (Tree t) where
    rmap = FMap
    lmap = LMap

(=.) :: (Profunctor p) => (a -> b) -> p b c -> p a c
(=.) = lmap
infixr 5 =.

cost :: Tree t i o -> Int
cost = \case
    FMap _ c     -> cost c
    LMap _ c     -> cost c
    Pure _       -> 0
    Apply c c'   -> cost c + cost c'
    Node _       -> 1
    Annotate _ c -> cost c

parser ::
    forall t i o.
    (forall a. t a -> Parser t a) ->
    Tree t i o ->
    Parser t o
parser alg = go
  where
    go :: forall i' o'. Tree t i' o' -> Parser t o'
    go (Pure x) s = (Right x, s)
    go (FMap f c) s = case go c s of
        (Left e, s')  -> (Left e, s')
        (Right x, s') -> (Right (f x), s')
    go (LMap _ c) s = go c s
    go (Apply cf cx) s = case go cf s of
        (Left e, s1)  -> (Left e, s1)
        (Right f, s1) -> case go cx s1 of
            (Left e, s2)  -> (Left e, s2)
            (Right x, s2) -> (Right (f x), s2)
    go (Node t) s = alg t s
    go (Annotate _ c) s = go c s

-- | Require full consumption — a generalization of
--   'Okapi.HTTP.Request.Path.parseExact' usable by any 'Tree' instance.
--   'Left' with the underlying error if parsing itself failed; 'Left' with
--   the leftover context if anything remains unconsumed.
parseExact ::
    (Eq (Context t), Monoid (Context t)) =>
    (Tree t i o -> Parser t o) ->
    Tree t i o -> Context t -> Either (Either (Failure t) (Context t)) o
parseExact p c ctx = case p c ctx of
    (Left e, _)                      -> Left (Left e)
    (Right a, rest) | rest == mempty -> Right a
                     | otherwise     -> Left (Right rest)

printer ::
    forall t i o.
    (Monoid (Context t)) =>
    (forall a. t a -> Printer t a) ->
    Tree t i o ->
    Printer t i
printer alg = go
  where
    go :: forall i' o'. Tree t i' o' -> Printer t i'
    go (Pure _) _      = mempty
    go (FMap _ c) i    = go c i
    go (LMap f c) i    = go c (f i)
    go (Apply cf cx) i = go cf i <> go cx i
    go (Node t) i      = alg t i
    go (Annotate _ c) i = go c i

-- | Print a value, appending @extra@ context after it. Not a law — a plain
--   utility, the same construction that 'printParseWith' feeds to its
--   parser.
printWith ::
    (Monoid (Context t)) =>
    (SymTree t a -> Printer t a) -> SymTree t a -> a -> Context t -> Context t
printWith q c x extra = q c x <> extra

-- | Xia, Orchard & Wang's Definition 4 ("backward round tripping",
--   /Composing bidirectional programs monadically/, ESOP 2019): print a
--   value with arbitrary trailing context appended ('printWith'), and
--   parsing gives back the value with that trailing context untouched.
--   Needs 'parseWith' to hold, which every parser in this codebase
--   satisfies by construction (consume-a-prefix discipline) — and is the
--   same fact that makes 'Okapi.HTTP.Request.Headers.fieldRFC9651''s
--   "leave the unconsumed remainder under the same header name" trick safe.
printParseWith ::
    (Eq a, Eq (Failure t), Eq (Context t), Monoid (Context t)) =>
    (SymTree t a -> Parser t a) ->
    (SymTree t a -> Printer t a) ->
    SymTree t a -> a -> Context t -> Bool
printParseWith p q c x extra = p c (printWith q c x extra) == (Right x, extra)

-- | The @extra = mempty@ specialization of 'printParseWith' — full
--   consumption, no leftover. Paired with 'parseExact' the way
--   'printParseWith' is paired with plain parsing: most single-value,
--   self-contained round trips want this, not the fully general form.
printParse ::
    (Eq a, Eq (Failure t), Eq (Context t), Monoid (Context t)) =>
    (SymTree t a -> Parser t a) ->
    (SymTree t a -> Printer t a) ->
    SymTree t a -> a -> Bool
printParse p q c x = printParseWith p q c x mempty

-- | Structural invariant every consume-a-prefix parser satisfies —
--   appending extra context after what's actually consumed doesn't
--   disturb what's consumed, it just rides along in the leftover.
--   Justifies 'printParseWith' above as a sound property to test at all.
parseWith ::
    (Eq a, Eq (Failure t), Eq (Context t), Monoid (Context t)) =>
    (SymTree t a -> Parser t a) -> SymTree t a -> Context t -> Context t -> Bool
parseWith p c ctx extra =
    p c (ctx <> extra) == (case p c ctx of (r, rest) -> (r, rest <> extra))

-- | Parse then print recovers the original context, fully (mod leftover) —
--   generalized over what happens when parsing the (usually arbitrary,
--   often un-parseable) test input fails outright. 'parsePrint' below
--   hardcodes @True@ (vacuously pass) — fine for a wide-grammar combinator
--   like @raw@, where nearly everything parses, but for a narrow grammar
--   (an integer, a token, a quoted string...) nearly every random input
--   fails to parse, and @True@ silently turns the test into "passed 100
--   tests" that barely tested anything. Pass QuickCheck's @discard@
--   instead of @True@ to make that visible — QuickCheck then tracks and
--   caps how many discards it'll tolerate before genuinely failing the
--   run, rather than counting them as passes. Dependency-free: @onParseFail@
--   is just a 'Bool' from this module's point of view, so it doesn't need
--   to import @QuickCheck@ to offer this — the caller (a doctest, which
--   does depend on it) supplies @discard@.
parsePrintOr ::
    (Eq (Context t), Monoid (Context t)) =>
    Bool ->
    (SymTree t a -> Parser t a) ->
    (SymTree t a -> Printer t a) ->
    SymTree t a -> Context t -> Bool
parsePrintOr onParseFail p q c ctx = case p c ctx of
    (Right x, rest) -> q c x <> rest == ctx
    (Left _, _)      -> onParseFail

-- | 'parsePrintOr' with the vacuous-pass-on-failure default — see there.
parsePrint ::
    (Eq (Context t), Monoid (Context t)) =>
    (SymTree t a -> Parser t a) ->
    (SymTree t a -> Printer t a) ->
    SymTree t a -> Context t -> Bool
parsePrint = parsePrintOr True

-- | Structural projection mirroring 'parser'/'printer': ignore bytes
--   entirely and track what happens to the /value/ as it flows through
--   'Pure'\/'FMap'\/'LMap'\/'Apply', delegating to a per-@t@ witness at
--   each 'Node'. For a well-behaved leaf the witness is 'id'; for a
--   'Node' wrapping a further 'Tree', delegate to that sub-tree's own
--   'purify'; for a 'Node' looping over several sub-parses, @map@/@traverse@
--   the sub-witness. Used to state 'printParseWeak'\/'parsePrintWeak'
--   and to reason about the compositionality of 'Apply'\/'FMap'\/'LMap'\/'Pure'
--   generically; not needed for testing any single exposed, aligned
--   combinator directly (see the strong properties above for that) — for
--   any 'Pure'-free tree with @witness = id@ at every leaf,
--   @purify id c i == i@ by induction on 'Apply'\/'FMap'\/'LMap'\/'Node',
--   which collapses 'printParseWeak' to exactly 'printParse'. The two
--   families only diverge for a tree that actually contains 'Pure' —
--   reachable by any consumer of this library via the public
--   'Applicative' instance, not just code internal to this package.
purify :: forall t i o. (forall a. t a -> a -> a) -> Tree t i o -> i -> o
purify witness = go
  where
    go :: forall i' o'. Tree t i' o' -> i' -> o'
    go (Pure x)      _ = x
    go (FMap f c)    i = f (go c i)
    go (LMap f c)    i = go c (f i)
    go (Apply cf cx) i = (go cf i) (go cx i)
    go (Node t)      i = witness t i
    go (Annotate _ c) i = go c i

-- | Weak version of 'printParseWith' at @extra = mempty@: compares against
--   'purify''s prediction instead of the caller's own @i@, so it holds for
--   generic combinators like 'Pure' (which 'printParse' cannot) and
--   composes through 'Apply'\/'FMap'\/'LMap'. Recovering 'printParse' from
--   this needs one extra, non-compositional fact checked once per concrete
--   tree: @purify witness c x == x@.
printParseWeak ::
    (Eq o, Eq (Failure t), Eq (Context t), Monoid (Context t)) =>
    (forall a. t a -> a -> a) ->
    (Tree t i o -> Parser t o) ->
    (Tree t i o -> Printer t i) ->
    Tree t i o -> i -> Bool
printParseWeak witness p q c i = p c (q c i) == (Right (purify witness c i), mempty)

-- | Weak version of 'parsePrint', dual of 'printParseWeak'. If parsing
--   @ctx@ yields exactly what 'purify' predicts for some @i@, printing
--   that @i@ (plus leftover) must reconstruct @ctx@.
parsePrintWeak ::
    (Eq o, Eq (Context t), Monoid (Context t)) =>
    (forall a. t a -> a -> a) ->
    (Tree t i o -> Parser t o) ->
    (Tree t i o -> Printer t i) ->
    Tree t i o -> i -> Context t -> Bool
parsePrintWeak witness p q c i ctx = case p c ctx of
    (Right o, rest) | o == purify witness c i -> q c i <> rest == ctx
    _                                          -> True

-- | Backward roundtrip for a no-leftover, bare @decode@\/@encode@ pair —
--   'Leaf''s own shape, and also 'Method'\/'Status'\/'Body'\'s @parse@\/@print@,
--   none of which go through 'Tree' at all.
leafPrintParse :: (Eq a, Eq e) => (c -> Either e a) -> (a -> c) -> a -> Bool
leafPrintParse decode encode x = decode (encode x) == Right x

-- | Forward roundtrip for a no-leftover pair, generalized over what
--   happens when @decode@ fails outright — see 'parsePrintOr', same
--   reasoning, same intended use (pass QuickCheck's @discard@ for a
--   narrow-grammar leaf).
leafParsePrintOr :: Eq c => Bool -> (c -> Either e a) -> (a -> c) -> c -> Bool
leafParsePrintOr onDecodeFail decode encode piece = case decode piece of
    Right x -> encode x == piece
    Left _  -> onDecodeFail

-- | 'leafParsePrintOr' with the vacuous-pass-on-failure default — see there.
leafParsePrint :: Eq c => (c -> Either e a) -> (a -> c) -> c -> Bool
leafParsePrint = leafParsePrintOr True

-- | Weaker than 'leafPrintParse': doesn't need @Eq a@, needs @Eq c@.
leafPrintParsePrint :: Eq c => (c -> Either e a) -> (a -> c) -> a -> Bool
leafPrintParsePrint decode encode x = case decode (encode x) of
    Right x' -> encode x' == encode x
    Left _   -> False

-- | Dual of 'leafPrintParsePrint': needs @Eq a@, not @Eq c@. Generalized
--   over what happens when the /first/ @decode@ fails outright (the
--   second, on @encode x@, staying a hard 'False' — that one failing
--   after the first succeeded is a genuine round-trip break, not just an
--   unparseable random input) — see 'parsePrintOr'.
leafParsePrintParseOr :: Eq a => Bool -> (c -> Either e a) -> (a -> c) -> c -> Bool
leafParsePrintParseOr onDecodeFail decode encode piece = case decode piece of
    Right x -> case decode (encode x) of
        Right x' -> x == x'
        Left _   -> False
    Left _ -> onDecodeFail

-- | 'leafParsePrintParseOr' with the vacuous-pass-on-failure default —
--   see there.
leafParsePrintParse :: Eq a => (c -> Either e a) -> (a -> c) -> c -> Bool
leafParsePrintParse = leafParsePrintParseOr True
