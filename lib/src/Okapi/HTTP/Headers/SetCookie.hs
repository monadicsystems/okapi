
module Okapi.HTTP.Headers.SetCookie (
    SetCookie,
    ParseError (..),
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (Day)
import Data.UUID (UUID)
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Piece)
import Web.HttpApiData (parseHeader, toHeader)

type SetCookie :: Type -> Type -> Type
data SetCookie i o

data ParseError = ParseError deriving (Eq, Show)

type instance Piece SetCookie = ByteString
type instance Failure SetCookie = ParseError

instance HasLeaf SetCookie Int where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf SetCookie Int16 where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf SetCookie Int32 where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf SetCookie Int64 where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int64"))
instance HasLeaf SetCookie Integer where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf SetCookie Bool where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "boolean" Nothing)
instance HasLeaf SetCookie Text where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" Nothing)
instance HasLeaf SetCookie UUID where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "uuid"))
instance HasLeaf SetCookie Day where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date"))

instance HasLeaf SetCookie ByteString where
    leaf = Leaf Right id (Info "string" Nothing)
