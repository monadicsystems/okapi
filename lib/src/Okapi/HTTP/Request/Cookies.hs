
module Okapi.HTTP.Request.Cookies (
    Cookie,
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

type Cookie :: Type -> Type
data Cookie a

data ParseError = ParseError deriving (Eq, Show)

type instance Piece Cookie = ByteString
type instance Failure Cookie = ParseError

instance HasLeaf Cookie Int where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Cookie Int16 where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf Cookie Int32 where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int32"))
instance HasLeaf Cookie Int64 where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" (Just "int64"))
instance HasLeaf Cookie Integer where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Cookie Bool where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "boolean" Nothing)
instance HasLeaf Cookie Text where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" Nothing)
instance HasLeaf Cookie UUID where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "uuid"))
instance HasLeaf Cookie Day where
    leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "string" (Just "date"))

instance HasLeaf Cookie ByteString where
    leaf = Leaf Right id (Info "string" Nothing)
