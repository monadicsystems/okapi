{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Pattern where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Atto
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Invariant
import Data.Map
import Data.Text
import GHC.IO.Handle.Types (Handle__)
import Okapi (ok)
import Okapi.Parser
import Okapi.Types
import Web.HttpApiData

data API
  = PostsRoute
  | PostRoute Int
  | PostEdit Int
  | PostBrowse Int Text

{-
newtype Matcher a = Matcher {unMatcher :: Text -> (Maybe a, Text)}

instance Functor Matcher where
  fmap :: (a -> b) -> Matcher a -> Matcher b
  fmap f (Matcher matcher) =
    Matcher $ \txt -> case matcher txt of
      (Nothing, _) -> (Nothing, txt)
      (Just val, txt') -> (Just $ f val, txt')

instance Applicative Matcher where
  pure :: a -> Matcher a
  pure val = Matcher $ \txt -> (Just val, txt)
  (<*>) :: Matcher (a -> b) -> Matcher a -> Matcher b
  (Matcher matcherF) <*> (Matcher matcherX) = Matcher $ \txt ->
    case matcherF txt of
      (Nothing, _) -> (Nothing, txt)
      (Just f, s') -> case matcherX s' of
        (Nothing, _) -> (Nothing, s')
        (Just x, s'') -> (Just $ f x, s'')

instance Alternative Matcher where
  empty :: Matcher a
  empty = Matcher $ \txt -> (Nothing, txt)
  (<|>) :: Matcher a -> Matcher a -> Matcher a
  (Matcher matcherL) <|> (Matcher matcherR) = Matcher $ \txt ->
    case matcherL txt of
      (Nothing, _) -> case matcherR txt of
        (Nothing, _) -> (Nothing, txt)
        successR -> successR
      successL -> successL

instance Monad Matcher where
  return = pure
  (>>=) :: Matcher a -> (a -> Matcher b) -> Matcher b
  (Matcher matcherA) >>= aToMatcherB = Matcher $ \txt ->
    case matcherA txt of
      (Nothing, _) -> (Nothing, txt)
      (Just val, txt') ->
        let finalMatcher = unMatcher $ aToMatcherB val
         in finalMatcher txt'
-}

newtype Printer a = Printer {unPrinter :: a -> Text}

segPrinter :: Printer Text
segPrinter = Printer $ \rawTxt -> "/" <> rawTxt

segParser :: Parser Text
segParser = do
  char '/'
  Atto.takeWhile1 (\c -> c /= '/' && c /= '?')

segPattern :: Pattern Text
segPattern = Pattern segParser segPrinter

(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided

data MyRoute = MyRoute Text Text Text
  deriving (Eq, Show)

myRoutePrinter :: Printer MyRoute
myRoutePrinter = adapt >$< segPrinter >*< segPrinter >*< segPrinter

adapt (MyRoute x y z) = ((x, y), z)

myRouteParser :: Parser MyRoute
myRouteParser = MyRoute <$> segParser <*> segParser <*> segParser

instance Contravariant Printer where
  contramap :: (b -> a) -> (Printer a -> Printer b)
  contramap f (Printer printer) = Printer (printer . f)

instance Divisible Printer where
  divide :: (a -> (b, c)) -> Printer b -> Printer c -> Printer a
  divide f (Printer g) (Printer h) = Printer $ \a -> case f a of
    (b, c) -> g b <> h c
  conquer = Printer $ const mempty

-- Add tuple parameter????
data Pattern a = Pattern
  { parser :: Parser a,
    printer :: Printer a
  }

instance Invariant Pattern where
  invmap :: (a -> b) -> (b -> a) -> Pattern a -> Pattern b
  invmap aToB bToA (Pattern parserA printerA) =
    Pattern
      { parser = fmap aToB parserA,
        printer = contramap bToA printerA
      }

{- Might not need
slash :: Pattern ()
slash = Pattern
  { matcher = Matcher $ \txt -> case Data.Text.uncons txt of
      Nothing -> (Nothing, txt)
      Just (head, txt') -> if head == '/' then (Just (), txt') else (Nothing, txt)
  , printer = Printer $ const "/"
  }
-}

pathSegRaw :: Pattern Text
pathSegRaw =
  Pattern
    { parser = do
        char '/'
        Atto.takeWhile1 (\c -> c /= '/' && c /= '?'),
      printer = Printer $ \rawTxt -> "/" <> rawTxt
    }

pathSeg :: (forall a. ToHttpApiData a, FromHttpApiData a) => Pattern a
pathSeg =
  Pattern
    { parser = do
        char '/'
        txt <- Atto.takeWhile1 (\c -> c /= '/' && c /= '?')
        case parseUrlPieceMaybe txt of
          Nothing -> fail "Couldn't parse path segment"
          Just value -> pure value,
      printer = Printer $ \value -> "/" <> toUrlPiece value
    }

queryParams :: Pattern (Map Text Text)
queryParams = undefined

{-
  Pattern
    { parser = do
        char '&' <|> char '?'
        Atto.takeWhile1 (/= '&'),
      printer = Printer $ \rawTxt -> "/" <> rawTxt
    }
-}

getRawURL :: MonadOkapi m => m Text
getRawURL = undefined

-- match :: MonadOkapi m => Pattern a -> m a
-- match pattern = do
--   rawURL <- getRawURL
--   maybe next return (matcher pattern rawURL)
