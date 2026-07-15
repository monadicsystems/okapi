{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.HTTP.Request.Query (
    Query (..),
    ArrayStyle (..),
    ParseError (..),
    parser,
    printer,
    parseExact,
    raw,
    param,
    param',
    param_,
    flag,
    flag',
    list,
    list',
    GQuery (..),
    derived,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1, Selector (..), (:*:) (..))
import Network.HTTP.Types qualified as Types
import Okapi.Tree (Failure, HasLeaf (..), Info (..), Leaf (..), Parser, Printer, Piece, Context, Tree (..))
import Okapi.Tree qualified as Tree
import Web.HttpApiData (parseQueryParam, toQueryParam)

-- $setup
-- >>> :set -XApplicativeDo
-- >>> import Okapi.Tree (printParse, int, integer, (=.))
-- >>> import Data.List.NonEmpty (NonEmpty((:|)))
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- let twoParams = do
--       x <- fst =. param "x" int
--       y <- snd =. param "y" int
--       pure (x, y)
--     verboseCount = do
--       flag "verbose"
--       x <- param "n" int
--       pure x
-- :}
--
-- >>> :{
-- data Filt = Filt { qX :: Int, qY :: Int } deriving (Generic, Eq, Show)
-- :}

type Query :: Type -> Type -> Type
data Query i o where
    Raw    :: Query Types.Query Types.Query
    Param  :: Text -> Leaf Query a -> Query a a
    Param' :: Text -> Leaf Query a -> Query (Maybe a) (Maybe a)
    Param_ :: Text -> Leaf Query a -> a -> Query i ()
    Flag   :: Text -> Query i ()
    Flag'  :: Text -> Query Bool Bool
    List   :: ArrayStyle -> Text -> Leaf Query a -> Query (NonEmpty a) (NonEmpty a)
    List'  :: ArrayStyle -> Text -> Leaf Query a -> Query [a] [a]

data ArrayStyle = Exploded | CommaDelimited | SpaceDelimited | PipeDelimited deriving (Eq, Show)

data ParseError = ParseError deriving (Eq, Show)

type instance Context Query = Types.Query
type instance Failure Query = ParseError
type instance Piece Query = Text

parser :: Tree Query i o -> Parser Query o
parser = Tree.parser alg
  where
    alg :: Query i o -> Parser Query o
    alg Raw q = (Right q, [])
    alg (Param key vLeaf) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)                 -> (Left ParseError, q)
            ((_, Nothing) : _, _)  -> (Left ParseError, q)
            ((_, Just v) : _, rest) -> case vLeaf.decode (decodeUtf8Lenient v) of
                Left e  -> (Left e, q)
                Right x -> (Right x, rest)
    alg (Param' key vLeaf) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)                  -> (Right Nothing, q)
            ((_, Nothing) : _, rest) -> (Right Nothing, rest)
            ((_, Just v) : _, rest)  -> case vLeaf.decode (decodeUtf8Lenient v) of
                Left _  -> (Right Nothing, rest)
                Right x -> (Right (Just x), rest)
    alg (Flag key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)       -> (Left ParseError, q)
            (_ : _, rest) -> (Right (), rest)
    alg (Param_ key vLeaf x) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ((_, Just v) : _, rest) | decodeUtf8Lenient v == vLeaf.encode x -> (Right (), rest)
            _ -> (Left ParseError, q)
    alg (Flag' key) q =
        case partition (\(k, _) -> k == encodeUtf8 key) q of
            ([], _)       -> (Right False, q)
            (_ : _, rest) -> (Right True, rest)
    alg (List style key vLeaf) q =
        let (vals, rest) = collectList style key q
         in case traverse (vLeaf.decode . decodeUtf8Lenient) vals of
                Left _   -> (Left ParseError, q)
                Right xs -> case NE.nonEmpty xs of
                    Nothing  -> (Left ParseError, q)
                    Just nel -> (Right nel, rest)
    alg (List' style key vLeaf) q =
        let (vals, rest) = collectList style key q
         in case traverse (vLeaf.decode . decodeUtf8Lenient) vals of
                Left _   -> (Left ParseError, q)
                Right xs -> (Right xs, rest)

printer :: Tree Query i o -> Printer Query i
printer = Tree.printer alg
  where
    alg :: Query i o -> Printer Query i
    alg Raw                    q        = q
    alg (Param key vLeaf)      x        = [(encodeUtf8 key, Just (encodeUtf8 (vLeaf.encode x)))]
    alg (Param' _ _)           Nothing  = []
    alg (Param' key vLeaf)     (Just x) = [(encodeUtf8 key, Just (encodeUtf8 (vLeaf.encode x)))]
    alg (Param_ key vLeaf x)   _        = [(encodeUtf8 key, Just (encodeUtf8 (vLeaf.encode x)))]
    alg (Flag key)             _        = [(encodeUtf8 key, Nothing)]
    alg (Flag' key)            True     = [(encodeUtf8 key, Nothing)]
    alg (Flag' _)              False    = []
    alg (List style key vLeaf)  nel     = renderList style key vLeaf (NE.toList nel)
    alg (List' style key vLeaf) xs      = renderList style key vLeaf xs

-- | Require full consumption — 'Left' with the leftover query params if
--   any remain, 'Left' with the underlying error if parsing itself failed.
parseExact :: Tree Query i o -> Types.Query -> Either (Either ParseError Types.Query) o
parseExact = Tree.parseExact parser

collectList :: ArrayStyle -> Text -> Types.Query -> ([ByteString], Types.Query)
collectList Exploded key q =
    let k = encodeUtf8 key
        (matched, rest) = partition (\(ik, _) -> ik == k) q
     in ([v | (_, Just v) <- matched], rest)
collectList style key q =
    let k = encodeUtf8 key
     in case partition (\(ik, _) -> ik == k) q of
            ([], _) -> ([], q)
            ((_, Nothing) : _, rest) -> ([], rest)
            ((_, Just v) : _, rest) -> (filter (not . BS.null) (BS8.split (delim style) v), rest)

renderList :: ArrayStyle -> Text -> Leaf Query a -> [a] -> Types.Query
renderList Exploded key vLeaf xs = [(encodeUtf8 key, Just (enc x)) | x <- xs]
  where
    enc = encodeUtf8 . vLeaf.encode
renderList style key vLeaf xs
    | null xs   = []
    | otherwise = [(encodeUtf8 key, Just (BS.intercalate (BS8.singleton (delim style)) (map enc xs)))]
  where
    enc = encodeUtf8 . vLeaf.encode

delim :: ArrayStyle -> Char
delim CommaDelimited = ','
delim SpaceDelimited = ' '
delim PipeDelimited  = '|'
delim Exploded       = ','

-- | Pass the raw query straight through, unconstrained.
--
-- >>> parser raw [("a", Just "1")]
-- (Right [("a",Just "1")],[])
-- >>> printer raw [("a", Just "1")]
-- [("a",Just "1")]
raw :: Tree Query Types.Query Types.Query
raw = Node Raw

-- | Parse and print a required query parameter.
--
-- prop> printParse parser printer (param "k" int) (x :: Int)
param :: Text -> Leaf Query a -> Tree Query a a
param key vLeaf = Node (Param key vLeaf)

-- | Parse and print an optional query parameter.
--
-- prop> printParse parser printer (param' "k" int) (x :: Maybe Int)
param' :: Text -> Leaf Query a -> Tree Query (Maybe a) (Maybe a)
param' key vLeaf = Node (Param' key vLeaf)

-- | A parameter constrained to one known value — parses only if present
--   and equal to the fixed value, ignored on print (the value is baked in).
--
-- >>> parser (param_ "v" int 1) [("v", Just "1")]
-- (Right (),[])
-- >>> parser (param_ "v" int 1) [("v", Just "2")]
-- (Left ParseError,[("v",Just "2")])
-- >>> printer (param_ "v" int 1) ()
-- [("v",Just "1")]
param_ :: Text -> Leaf Query a -> a -> Tree Query i ()
param_ key vLeaf x = Node (Param_ key vLeaf x)

-- | Parse and print a boolean flag (presence = True).
--
-- prop> printParse parser printer (flag "f") ()
-- prop> printParse parser printer (flag' "f") (x :: Bool)
--
-- Composes directly with a value-producing sibling in the same @do@\/
-- 'Applicative' block — no explicit alignment needed, since its own input
-- is unconstrained (see 'verboseCount' in "$setup"):
--
-- >>> parser verboseCount [("verbose", Nothing), ("n", Just "5")]
-- (Right 5,[])
-- >>> printer verboseCount 5
-- [("verbose",Nothing),("n",Just "5")]
--
-- prop> printParse parser printer verboseCount (x :: Int)
flag :: Text -> Tree Query i ()
flag key = Node (Flag key)

flag' :: Text -> Tree Query Bool Bool
flag' key = Node (Flag' key)

-- | Parse and print a non-empty list of query parameters.
--
-- prop> printParse parser printer (list Exploded "k" int) (x :| xs)
-- prop> printParse parser printer (list CommaDelimited "k" int) (x :| xs)
list :: ArrayStyle -> Text -> Leaf Query a -> Tree Query (NonEmpty a) (NonEmpty a)
list style key vLeaf = Node (List style key vLeaf)

-- | Like 'list', but accepts an empty (possibly-empty) list instead of
--   requiring at least one value.
--
-- >>> parser (list' Exploded "k" int) []
-- (Right [],[])
-- >>> printer (list' Exploded "k" int) []
-- []
-- >>> parser (list' Exploded "k" int) [("k", Just "1"), ("k", Just "2")]
-- (Right [1,2],[])
-- >>> printer (list' Exploded "k" int) [1, 2]
-- [("k",Just "1"),("k",Just "2")]
list' :: ArrayStyle -> Text -> Leaf Query a -> Tree Query [a] [a]
list' style key vLeaf = Node (List' style key vLeaf)

instance HasLeaf Query Int        where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" Nothing)
instance HasLeaf Query Int16      where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" (Just "int32"))
instance HasLeaf Query Int32      where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" (Just "int32"))
instance HasLeaf Query Int64      where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" (Just "int64"))
instance HasLeaf Query Integer    where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "integer" Nothing)
instance HasLeaf Query Bool       where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "boolean" Nothing)
instance HasLeaf Query Float      where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "number" (Just "float"))
instance HasLeaf Query Double     where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "number" (Just "double"))
instance HasLeaf Query Text       where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" Nothing)
instance HasLeaf Query UUID       where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" (Just "uuid"))
instance HasLeaf Query Day        where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" (Just "date"))
instance HasLeaf Query UTCTime    where leaf = Leaf (first (const ParseError) . parseQueryParam) toQueryParam (Info "string" (Just "date-time"))

class GQuery (f :: Type -> Type) where
    gQueryCodec :: Tree Query (f ()) (f ())

instance (GQuery f) => GQuery (D1 meta f) where
    gQueryCodec = FMap M1 $ LMap unM1 gQueryCodec

instance (GQuery f) => GQuery (C1 meta f) where
    gQueryCodec = FMap M1 $ LMap unM1 gQueryCodec

instance (GQuery f, GQuery g) => GQuery (f :*: g) where
    gQueryCodec =
        Apply
            (FMap (\l r -> l :*: r) (LMap (\(l :*: _) -> l) gQueryCodec))
            (LMap (\(_ :*: r) -> r) gQueryCodec)

instance (Selector s, HasLeaf Query a) => GQuery (S1 s (Rec0 a)) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 a) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Param key (leaf @Query @a))

instance {-# OVERLAPPING #-} (Selector s, HasLeaf Query a) => GQuery (S1 s (Rec0 (Maybe a))) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 (Maybe a)) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Param' key (leaf @Query @a))

instance {-# OVERLAPPING #-} (Selector s) => GQuery (S1 s (Rec0 ())) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 ()) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Flag key)

instance {-# OVERLAPPING #-} (Selector s) => GQuery (S1 s (Rec0 Bool)) where
    gQueryCodec =
        let key = Text.pack (selName (undefined :: S1 s (Rec0 Bool) ()))
         in FMap (M1 . K1) $ LMap (unK1 . unM1) $ Node (Flag' key)

-- | Generically derive a 'Query' codec from a record's field names and
--   'HasLeaf' instances.
--
-- >>> parser (derived @Filt) [("qX", Just "1"), ("qY", Just "2")]
-- (Right (Filt {qX = 1, qY = 2}),[])
-- >>> printer (derived @Filt) (Filt 1 2)
-- [("qX",Just "1"),("qY",Just "2")]
derived :: forall a. (Generic a, GQuery (Rep a)) => Tree Query a a
derived = FMap (to @a) $ LMap (from @a) gQueryCodec

-- $combined
-- >>> parser twoParams [("x", Just "1"), ("y", Just "2"), ("z", Just "3")]
-- (Right (1,2),[("z",Just "3")])
--
-- prop> printParse parser printer twoParams (xy :: (Int, Int))
