module Okapi.Result where

data Result e a = Ok a | Error e

instance Functor (Result e) where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Error e) = Error e

instance Semigroup e => Applicative (Result e) where
  pure = Ok
  Error e1 <*> b = Error $ case b of
    Error e2 -> e1 <> e2
    Ok _ -> e1
  Ok _ <*> Error e2 =
    Error e2
  Ok f <*> Ok a =
    Ok (f a)
