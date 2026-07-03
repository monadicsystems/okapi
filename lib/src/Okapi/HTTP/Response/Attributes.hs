
module Okapi.HTTP.Response.Attributes (
    Attributes,
    parser,
    printer,
    attribute,
    attribute',
    flag,
    flag',
    raw,
    maxAge,
    domain,
    path,
    secure,
    httpOnly,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context)
import Okapi.Tree qualified as Tree
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Web.HttpApiData (parseHeader, toHeader)

type Attributes :: Type -> Type
data Attributes a where
    Attr  :: ByteString -> Leaf Attributes a -> Attributes a
    Attr' :: ByteString -> Leaf Attributes a -> Attributes (Maybe a)
    Flag  :: ByteString -> Attributes ()
    Flag' :: ByteString -> Attributes Bool
    Raw   :: Attributes ByteString

data ParseError = ParseError deriving (Eq, Show)

type instance Piece Attributes = ByteString
type instance Context Attributes = ByteString
type instance Failure Attributes = ParseError

instance HasLeaf Attributes Int     where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Attributes Integer where leaf = Leaf (first (const ParseError) . parseHeader) toHeader (Info "integer" Nothing)
instance HasLeaf Attributes ByteString where leaf = Leaf Right id (Info "string" Nothing)

attribute :: ByteString -> Leaf Attributes a -> Tree Attributes a a
attribute key vLeaf = Node (Attr key vLeaf)

attribute' :: ByteString -> Leaf Attributes a -> Tree Attributes (Maybe a) (Maybe a)
attribute' key vLeaf = Node (Attr' key vLeaf)

flag :: ByteString -> Tree Attributes () ()
flag = Node . Flag

flag' :: ByteString -> Tree Attributes Bool Bool
flag' = Node . Flag'

raw :: Tree Attributes ByteString ByteString
raw = Node Raw

maxAge :: Tree Attributes Int Int
maxAge = attribute "Max-Age" (leaf @Attributes @Int)

domain :: Tree Attributes ByteString ByteString
domain = attribute "Domain" (leaf @Attributes @ByteString)

path :: Tree Attributes ByteString ByteString
path = attribute "Path" (leaf @Attributes @ByteString)

secure :: Tree Attributes () ()
secure = flag "Secure"

httpOnly :: Tree Attributes () ()
httpOnly = flag "HttpOnly"

parser :: Tree Attributes i o -> Parser Attributes o
parser = Tree.parser alg
  where
    alg :: Attributes a -> Parser Attributes a
    alg (Attr key vLeaf) s = case look key s of
        Just (Just v) -> case vLeaf.decode v of
            Left _  -> (Left ParseError, s)
            Right x -> (Right x, s)
        _ -> (Left ParseError, s)
    alg (Attr' key vLeaf) s = case look key s of
        Just (Just v) -> (Right (either (const Nothing) Just (vLeaf.decode v)), s)
        _             -> (Right Nothing, s)
    alg (Flag key) s = case look key s of
        Just _  -> (Right (), s)
        Nothing -> (Left ParseError, s)
    alg (Flag' key) s = (Right (maybe False (const True) (look key s)), s)
    alg Raw s = (Right s, s)

printer :: Tree Attributes i o -> Printer Attributes i
printer = Tree.printer alg
  where
    alg :: Attributes a -> Printer Attributes a
    alg (Attr key vLeaf)  v        = "; " <> key <> "=" <> vLeaf.encode v
    alg (Attr' key vLeaf) (Just v) = "; " <> key <> "=" <> vLeaf.encode v
    alg (Attr' _ _)       Nothing  = ""
    alg (Flag key)        ()       = "; " <> key
    alg (Flag' key)       True     = "; " <> key
    alg (Flag' _)         False    = ""
    alg Raw               bs       = bs

look :: ByteString -> ByteString -> Maybe (Maybe ByteString)
look key s = fmap snd (find ((== lc key) . fst) entries)
  where
    entries = [ breakEq (strip a) | a <- BS.split 59 s, not (BS.null (strip a)) ]
    breakEq a = case BS.elemIndex 61 a of
        Nothing -> (lc (strip a), Nothing)
        Just i  -> (lc (strip (BS.take i a)), Just (strip (BS.drop (i + 1) a)))

lc :: ByteString -> ByteString
lc = BS8.map toLower

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9
