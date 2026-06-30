{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.Leaf (
    StateOf,
    ErrorOf,
    PieceOf,
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
) where

import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)

type StateOf :: (Type -> Type) -> Type
type family StateOf t

type ErrorOf :: (Type -> Type) -> Type
type family ErrorOf t

type PieceOf :: (Type -> Type) -> Type
type family PieceOf t

type Parser t a = StateOf t -> (Either (ErrorOf t) a, StateOf t)
type Printer t a = a -> StateOf t

data Info = Info
    { typeName :: Text
    , format   :: Maybe Text
    }
    deriving (Eq, Show)

type Leaf :: (Type -> Type) -> Type -> Type
data Leaf t a = Leaf
    { decode :: PieceOf t -> Either (ErrorOf t) a
    , encode :: a -> PieceOf t
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
