
module Okapi.HTTP.RFC9651.Dictionary (
    Dictionary,
    parser,
    printer,
    member,
    member',
    list,
    list',
    raw,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.List (find)
import Data.Word (Word8)
import Okapi.Tree (Failure, Leaf, Parser, Printer, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.RFC9651.BareItem (BareItem, parseInnerToList, renderInner)
import Okapi.HTTP.RFC9651.Item (Item)
import Okapi.HTTP.RFC9651.Item qualified as Item

data ParseError = ParseError deriving (Eq, Show)

type Key = ByteString

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

firstTop :: Word8 -> ByteString -> Maybe Int
firstTop sep bs = go False (0 :: Int) 0
  where
    n = BS.length bs
    go inQ depth i
        | i >= n = Nothing
        | otherwise =
            let w = BS.index bs i
            in if w == 34 then go (not inQ) depth (i + 1)
               else if inQ then go inQ depth (i + 1)
               else if w == 40 then go inQ (depth + 1) (i + 1)
               else if w == 41 then go inQ (max 0 (depth - 1)) (i + 1)
               else if w == sep && depth == 0 then Just i
               else go inQ depth (i + 1)

splitTop :: Word8 -> ByteString -> [ByteString]
splitTop sep bs = case firstTop sep bs of
    Nothing -> [bs]
    Just i  -> BS.take i bs : splitTop sep (BS.drop (i + 1) bs)

breakKey :: ByteString -> (ByteString, Maybe ByteString)
breakKey bs = case firstTop 61 bs of
    Nothing -> (strip bs, Nothing)
    Just i  -> (strip (BS.take i bs), Just (strip (BS.drop (i + 1) bs)))

memberEntries :: ByteString -> [(ByteString, Maybe ByteString)]
memberEntries = map breakKey . filter (not . BS.null) . map strip . splitTop 44

type Dictionary :: Type -> Type
data Dictionary a where
    Member  :: Key -> Tree Item a a  -> Dictionary a
    Member' :: Key -> Tree Item a a  -> Dictionary (Maybe a)
    List    :: Key -> Leaf BareItem a -> Dictionary [a]
    List'   :: Key -> Leaf BareItem a -> Dictionary (Maybe [a])
    Raw     :: Dictionary ByteString

type instance Context Dictionary = ByteString
type instance Failure Dictionary = ParseError

member :: Key -> Tree Item a a -> Tree Dictionary a a
member k c = Node (Member k c)

member' :: Key -> Tree Item a a -> Tree Dictionary (Maybe a) (Maybe a)
member' k c = Node (Member' k c)

list :: Key -> Leaf BareItem a -> Tree Dictionary [a] [a]
list k vLeaf = Node (List k vLeaf)

list' :: Key -> Leaf BareItem a -> Tree Dictionary (Maybe [a]) (Maybe [a])
list' k vLeaf = Node (List' k vLeaf)

raw :: Tree Dictionary ByteString ByteString
raw = Node Raw

parser :: Tree Dictionary i o -> Parser Dictionary o
parser = Tree.parser alg
  where
    alg :: Dictionary a -> Parser Dictionary a
    alg t s = case t of
        Member key c -> case look s key of
            Just (Just v) -> case fst (Item.parser c v) of
                Left _  -> (Left ParseError, s)
                Right a -> (Right a, s)
            Just Nothing  -> case fst (Item.parser c "?1") of
                Left _  -> (Left ParseError, s)
                Right a -> (Right a, s)
            Nothing       -> (Left ParseError, s)
        Member' key c -> case look s key of
            Just (Just v) -> case fst (Item.parser c v) of
                Left _  -> (Right Nothing, s)
                Right a -> (Right (Just a), s)
            _             -> (Right Nothing, s)
        List key vLeaf -> case look s key of
            Just (Just v) -> case parseInnerToList vLeaf v of
                Left _   -> (Left ParseError, s)
                Right xs -> (Right xs, s)
            _             -> (Left ParseError, s)
        List' key vLeaf -> case look s key of
            Just (Just v) -> case parseInnerToList vLeaf v of
                Left _   -> (Right Nothing, s)
                Right xs -> (Right (Just xs), s)
            _             -> (Right Nothing, s)
        Raw -> (Right s, s)
      where
        look bs k = fmap snd (find ((== k) . fst) (memberEntries bs))

printer :: Tree Dictionary i o -> Printer Dictionary i
printer = Tree.printer alg
  where
    alg :: Dictionary a -> Printer Dictionary a
    alg (Member key c)      a         = ", " <> key <> "=" <> Item.printer c a
    alg (Member' key c)     (Just a)  = ", " <> key <> "=" <> Item.printer c a
    alg (Member' _ _)       Nothing   = ""
    alg (List key vLeaf)    xs        = ", " <> key <> "=" <> renderInner vLeaf xs
    alg (List' key vLeaf)   (Just xs) = ", " <> key <> "=" <> renderInner vLeaf xs
    alg (List' _ _)         Nothing   = ""
    alg Raw                 bs        = bs
