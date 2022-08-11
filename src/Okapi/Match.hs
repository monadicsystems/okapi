{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Okapi.Match where

import Data.Text
import Okapi.Parser
import Okapi.Types

data API
  = PostsRoute
  | PostRoute Int
  | PostEdit Int
  | PostBrowse Int Text

newtype Matcher a = Matcher { unMatcher :: Text -> (Maybe a, Text) }

instance Functor Matcher where
  fmap :: (a -> b) -> Matcher a -> Matcher b
  fmap f (Matcher matcher) =
    Matcher $ \txt -> case matcher txt of
      (Nothing, _) -> (Nothing, txt)
      (Just val, txt') -> (f val, txt') 

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
        successR -> (Just val, txt')
      successL -> successL

instance Monad Matcher where
  return = pure
  (>>=) :: Matcher a -> (a -> Matcher b) -> Matcher b
  (Matcher matcherA) >>= aToMatcherB = Matcher $ \txt ->
    case matcherA txt of
      (Nothing, _) -> (Nothing, txt)
      (Just val, txt') -> aToMatcherB val $ txt'

newtype Formatter a = Formatter { unFormatter :: a -> Text }

instance Contravariant Formatter where

data Pattern a = Pattern
  { matcher :: Matcher a,
    formatter :: Formatter a
  }

instance Functor Pattern where
  fmap :: (a -> b) -> Pattern a -> Pattern b
  fmap f pattern =
    Pattern
      { matcher = \txt -> case matcher pattern txt of
          Nothing -> Nothing
          Just val -> Just $ f val
      , formatter = \val -> 
      }

getRawURL :: MonadOkapi m => m Text
getRawURL = undefined

match :: MonadOkapi m => Pattern a -> m a
match pattern = do
  rawURL <- getRawURL
  maybe skip return (matcher pattern rawURL)
