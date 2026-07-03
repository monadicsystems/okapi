
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
    (=.),
    cost,
    parser,
    printer,
    printParse,
    printStable,
) where

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
    FMap  :: (o -> o') -> Tree t i o -> Tree t i o'
    LMap  :: (i -> i') -> Tree t i' o -> Tree t i o
    Pure  :: o -> Tree t i o
    Apply :: Tree t i (o -> o') -> Tree t i o -> Tree t i o'
    Node  :: t a -> Tree t a a

-- | A symmetrical @Tree t i o@ where @i ~ o@.
type SymTree t a = Tree t a a

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
    FMap _ c   -> cost c
    LMap _ c   -> cost c
    Pure _     -> 0
    Apply c c' -> cost c + cost c'
    Node _     -> 1

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

printParse ::
    (Eq a, Eq (Failure t), Eq (Context t), Monoid (Context t)) =>
    (SymTree t a -> Parser t a) ->
    (SymTree t a -> Printer t a) ->
    SymTree t a -> a -> Bool
printParse p q c x = p c (q c x) == (Right x, mempty)

printStable ::
    (Eq (Context t), Monoid (Context t)) =>
    (SymTree t a -> Parser t a) ->
    (SymTree t a -> Printer t a) ->
    SymTree t a -> a -> Bool
printStable p q c x = case p c (q c x) of
    (Right x', _) -> q c x' == q c x
    _             -> False
