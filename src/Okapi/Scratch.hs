{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Okapi.Scratch where

import Control.Applicative (Alternative(..))
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Control.Applicative.Combinators as Combinators
import qualified Control.Applicative.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wai.Parse as WAI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

{-|
The @Extraction@ data type represents the result of an @Extractor@.
An @Extracion@ is either @Ok@ or a @Error@.
-}

data Extraction e a = Ok a | Error e
  deriving (Eq, Show)

instance Functor (Extraction e) where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Error errors) = Error errors

instance Semigroup e => Applicative (Extraction e) where
  pure = Ok
  Error e1 <*> b = Error $ case b of
    Error e2 -> e1 <> e2
    Ok _ -> e1
  Ok _  <*> Error e2 =
    Error e2
  Ok f  <*> Ok a  =
    Ok (f a)

{-|
An @Extractor@ is like a parser pretty much.
It is backtracking by default.
Only Applicative.
The idea is that hopefully we can retain the structure of an Extractor.
It accumulates errors across alternatives.
-}
data Extractor s e a = Extractor { run :: s -> (Extraction e a, s) }

{-
data Extractor' e a =
  Method HTTP.Method
  | Path [Text.Text]
-}

instance Functor (Extractor s e) where
  fmap f extractor = Extractor $ \s ->
    let
      (extraction, s') = run extractor $ s
    in
      (fmap f extraction, s')

instance Semigroup e => Applicative (Extractor s e) where
  pure x = Extractor $ \s -> (pure x, s)
  f <*> x = Extractor $ \s ->
    case run f $ s of
      (Ok f', s') -> case run x $ s' of
        (Ok x', s'') -> (Ok $ f' x', s'')
        (Error errors, s'') -> (Error errors, s'')
      (Error errors, s') -> (Error $ errors, s')

{- Barrels through and accumulates errors and state:
      (Error errors, s') -> case run x $ s' of
        (Ok x', s'') -> (Error errors, s'')
        (Error errors', s'') -> (Error $ errors <> errors', s'')
-}

instance Monoid e => Alternative (Extractor s e) where
  empty = Extractor $ \s -> (Error mempty, s)
  ex1 <|> ex2 = Extractor $ \s ->
    case run ex1 $ s of
      (Error errors, _) -> case run ex2 $ s of
        (Error errors', s') -> (Error $ errors <> errors', s')
        (ok, s') -> (ok, s')
      (ok, s') -> (ok, s')

-- Extractor Operations
{-
get :: Extractor s e s
get = Extractor $ \s -> (Ok s, s)

gets :: (s -> a) -> Extractor s e a
gets f = Extractor $ \s -> (Ok $ f s, s)

put :: s -> Extractor s e ()
put new = Extractor $ \s -> (Ok (), new)

modify :: (s -> s) -> Extractor s e ()
modify f = Extractor $ \s -> (Ok (), f s)

abort :: e -> Extractor s e a
abort errors = Extractor $ \s -> (Error errors, s)
-}
{- Request Extractor -}

data RequestBody =
  RequestBodyRaw LBS.ByteString
  | RequestBodyMultipart ([WAI.Param], [WAI.File LBS.ByteString])
  deriving (Eq, Show)

data ParserError =
  InvalidJSON
  | InvalidMethod

type Parser = Extractor (WAI.Request, RequestBody) [ParserError]

-- | A @Parser a@ is an option parser returning a value of type 'a'.
{-
data Parser a
  = NilP (Maybe a)
  | OptP (Option a)
  | forall x . MultP (Parser (x -> a)) (Parser x)
  | AltP (Parser a) (Parser a)
  | forall x . BindP (Parser x) (x -> Parser a)
-}

type RequestInfo = (WAI.Request, RequestBody)

{-
data Xtractor a
  = PureX { pureX :: RequestInfo -> (Extraction ParserError a, RequestInfo) }
  | GetX { getX :: RequestInfo -> (Extraction ParserError RequestInfo, RequestInfo) }
  | PutX { putX :: RequestInfo -> (Extraction ParserError (), RequestInfo) }
  | AltX (Xtractor a) (Xtractor a)
  | forall x. ApplyX (Xtractor (x -> a)) (Xtractor a)
-}
  -- | forall x. IsX { goal :: x, errors :: [ParserError], isX :: RequestInfo -> (Extraction ParserError (), RequestInfo) }
  -- | forall x. SatisfyX { }

data Xtractor s e a where
  FMap :: (a -> b) -> Xtractor s e a -> Xtractor s e b
  Pure :: a -> Xtractor s e a
  App :: Xtractor s e (a -> b) -> Xtractor s e a -> Xtractor s e b
  Alt :: Xtractor s e a -> Xtractor s e a -> Xtractor s e a
  Abort :: Monoid e => e -> Xtractor s e a
  Get :: Xtractor s e s
  Put :: s -> Xtractor s e ()
  Modify :: (s -> s) -> Xtractor s e ()
  Is :: (Eq a, Monoid e) => e -> a -> Xtractor s e a -> Xtractor s e ()
  Satisfy :: Monoid e => e -> (a -> Bool) -> Xtractor s e a -> Xtractor s e ()

-- instance Show (Xtractor s e a) where

runXtractor :: Monoid e => Xtractor s e a -> s -> (Extraction e a, s)
runXtractor p s = case p of
  FMap f p' ->
    let
      (ex, s') = runXtractor p' s
    in
      (fmap f ex, s')
  Pure x -> (Ok x, s)
  App f x -> case runXtractor f s of
    (Ok f', s') -> case runXtractor x  s' of
      (Ok x', s'') -> (Ok $ f' x', s'')
      (Error errors, s'') -> (Error errors, s'')
    (Error errors, s') -> (Error $ errors, s')
  Alt ex1 ex2 -> case runXtractor ex1 s of
      (Error errors, _) -> case runXtractor ex2 s of
        (Error errors', s') -> (Error $ errors <> errors', s')
        (ok, s') -> (ok, s')
      (ok, s') -> (ok, s')
  Abort errors -> (Error errors, s)
  Get -> (Ok s, s)
  Put s' -> (Ok (), s')
  Modify f -> (Ok (), f s)
  Is errors goal xtr -> case runXtractor xtr s of
    (Ok x, s') -> if x == goal
      then (Ok (), s')
      else (Error errors, s')
    (Error errors', s') -> (Error $ errors' <> errors, s')
  Satisfy errors pred xtr -> case runXtractor xtr s of
    (Ok x, s') -> if pred x
      then (Ok (), s')
      else (Error errors, s')
    (Error errors', s') -> (Error $ errors' <> errors, s')

{-
method :: Parser HTTP.Method
method = do
  m <- gets (WAI.requestMethod . fst)
  modify (\(request, requestBody) -> (request { WAI.requestMethod = mempty }, requestBody))
  pure m
-}

{- Counter Extractor Example -}

type Counter = Xtractor Int [CounterError]

data CounterError = DivByZero | NotEqual
  deriving (Eq, Show)

get :: Xtractor s e s
get = Get

put :: s -> Xtractor s e ()
put = Put

modify :: (s -> s) -> Xtractor s e ()
modify f = Modify f

abort :: Monoid e => e -> Xtractor s e a
abort = Abort

is :: (Eq a, Monoid e) => e -> a -> Xtractor s e a -> Xtractor s e ()
is = Is

satisfy :: Monoid e => e -> (a -> Bool) -> Xtractor s e a -> Xtractor s e ()
satisfy = Satisfy

safeDiv :: Int -> Counter ()
safeDiv divisor =
  if divisor == 0
    then abort [DivByZero]
    else do
      modify (\s -> s `div` divisor)
      return ()

myCounter :: Counter ()
myCounter = do
  modify (+1)
  modify (*3)
  modify (\x -> x `div` 2)
  count <- get
  modify (*3)
  satisfy [NotEqual] (\x -> x > 50) get <|> do
    put 73
  pure ()

{-
type Counter = Extractor Int [CounterError]

-}