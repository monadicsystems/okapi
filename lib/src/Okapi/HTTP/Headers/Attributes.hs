{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Okapi.HTTP.Headers.Attributes (
    Attributes,
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
    parse,
    print,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (find)
import Okapi.Leaf (ErrorOf, HasLeaf (..), Leaf (..), StateOf)
import Okapi.Tree (Tree (..))
import Okapi.Tree qualified as Tree
import Okapi.HTTP.Headers.Cookies (Cookie, ParseError (..))
import Prelude hiding (print)

type Attributes :: Type -> Type
data Attributes a where
    Attr  :: ByteString -> Leaf Cookie a -> Attributes a
    Attr' :: ByteString -> Leaf Cookie a -> Attributes (Maybe a)
    Flag  :: ByteString -> Attributes ()
    Flag' :: ByteString -> Attributes Bool
    Raw   :: Attributes ByteString

type instance StateOf Attributes = ByteString
type instance ErrorOf Attributes = ParseError

attribute :: ByteString -> Leaf Cookie a -> Tree Attributes a a
attribute key vLeaf = Node (Attr key vLeaf)

attribute' :: ByteString -> Leaf Cookie a -> Tree Attributes (Maybe a) (Maybe a)
attribute' key vLeaf = Node (Attr' key vLeaf)

flag :: ByteString -> Tree Attributes () ()
flag = Node . Flag

flag' :: ByteString -> Tree Attributes Bool Bool
flag' = Node . Flag'

raw :: Tree Attributes ByteString ByteString
raw = Node Raw

maxAge :: Tree Attributes Int Int
maxAge = attribute "Max-Age" (leaf @Cookie @Int)

domain :: Tree Attributes ByteString ByteString
domain = attribute "Domain" (leaf @Cookie @ByteString)

path :: Tree Attributes ByteString ByteString
path = attribute "Path" (leaf @Cookie @ByteString)

secure :: Tree Attributes () ()
secure = flag "Secure"

httpOnly :: Tree Attributes () ()
httpOnly = flag "HttpOnly"

parse :: Tree Attributes i o -> ByteString -> (Either ParseError o, ByteString)
parse = Tree.grow alg
  where
    alg :: forall a. Attributes a -> ByteString -> (Either ParseError a, ByteString)
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

print :: Tree Attributes i o -> i -> ByteString
print = Tree.eat pr
  where
    pr :: forall a. Attributes a -> a -> ByteString
    pr (Attr key vLeaf)  v        = "; " <> key <> "=" <> vLeaf.encode v
    pr (Attr' key vLeaf) (Just v) = "; " <> key <> "=" <> vLeaf.encode v
    pr (Attr' _ _)       Nothing  = ""
    pr (Flag key)        ()       = "; " <> key
    pr (Flag' key)       True     = "; " <> key
    pr (Flag' _)         False    = ""
    pr Raw               bs       = bs

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
