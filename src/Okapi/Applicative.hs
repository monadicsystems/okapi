{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedRecordDotSyntax #-}

module Okapi.Applicative where

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

data Result e a = Ok a | Error e

instance Functor (Result e) where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Error e) = Error e

instance Semigroup e => Applicative (Result e) where
  pure = Ok
  Error e1 <*> b = Error $ case b of
    Error e2 -> e1 <> e2
    Ok _ -> e1
  Ok _  <*> Error e2 =
    Error e2
  Ok f  <*> Ok a  =
    Ok (f a)

data Extractor e a where
  -- | Core
  FMap :: (a -> b) -> Extractor e a -> Extractor e b
  Pure :: a -> Extractor e a
  App :: Extractor e (a -> b) -> Extractor e a -> Extractor e b
  Alt :: Extractor e a -> Extractor e a -> Extractor e a

  -- | API
  Method :: Monoid e => Extractor e HTTP.Method
  PathParam :: (Monoid e, Web.FromHttpApiData a) => Extractor e a
  -- Make it so order of PathParam doesn't matter.
  -- Can do this by adding an arg of Int to specify the index.
  -- Path :: Web.FromHttpApiData a => Extractor e [a]
  -- Path can be defined in terms of path param
  QueryParam :: Web.FromHttpApiData a => BS.ByteString -> Extractor e a
  QueryFlag :: BS.ByteString -> Extractor e ()
  Query :: Extractor e [(BS.ByteString, Maybe BS.ByteString)] -- ? Use Non-Empty
  Header :: HTTP.HeaderName -> Extractor e BS.ByteString
  Headers :: Extractor e [HTTP.Header] -- ? Use Non-Empty
  JSON :: Aeson.FromJSON a => Extractor e a
  FormParam :: Web.FromHttpApiData a => BS.ByteString -> Extractor e a
  Form :: Web.FromForm a => Extractor e a
  File :: forall e c. BS.ByteString -> Extractor e (WAI.FileInfo c)
  Raw :: Extractor e LBS.ByteString

  -- | Helpers
  Now :: Extractor e RequestInfo
  Abort :: Monoid e => e -> Extractor e a
  Is :: (Monoid e, Eq a) => a -> Extractor e a -> Extractor e ()
  -- TODO: Switch order of args for `Satisfy` and `Is`???
  Satisfy :: Monoid e => (a -> Bool) -> Extractor e a -> Extractor e ()
  -- Assert :: Monoid e => (RequestInfo -> Bool) -> Extractor e ()
  -- Replace with Is (Now ...)

instance Semigroup e => Functor (Extractor e) where
  fmap = FMap

instance Semigroup e => Applicative (Extractor e) where
  pure = Pure
  (<*>) = App

instance Monoid e => Alternative (Extractor e) where
  empty = Abort mempty
  (<|>) = Alt

data RequestBody =
  RequestBodyRaw LBS.ByteString
  | RequestBodyMultipart ([WAI.Param], [WAI.File LBS.ByteString])
  deriving (Eq, Show)

type RequestInfo = (WAI.Request, RequestBody)

extract :: Semigroup e => Extractor e a -> RequestInfo -> (Result e a, RequestInfo)
extract extractor requestInfo = case extractor of
  -- | Core
  FMap f extractorX ->
    let
      (result, requestInfo') = extract extractorX requestInfo
    in
      (fmap f result, requestInfo')
  Pure x -> (Ok x, requestInfo)
  App extractorF extractorX -> case extract extractorF requestInfo of
    (Ok f, requestInfo') -> case extract extractorX  requestInfo' of
      (Ok x, requestInfo'') -> (Ok $ f x, requestInfo'')
      (Error e, requestInfo'') -> (Error e, requestInfo'')
    (Error e, requestInfo') -> (Error e, requestInfo')
  Alt extractor1 extractor2 -> case extract extractor1 requestInfo of
      (Error e, _) -> case extract extractor2 requestInfo of
        (Error e', requestInfo') -> (Error $ e <> e', requestInfo')
        (ok, requestInfo') -> (ok, requestInfo')
      (ok, requestInfo') -> (ok, requestInfo')

  -- | API
  Method -> case WAI.requestMethod $ fst requestInfo of
    "" -> (Error mempty, requestInfo)
    m -> (Ok m, (\(request, requestBody) -> (request { WAI.requestMethod = mempty }, requestBody)) requestInfo)
    -- Should check if valid request method here
  PathParam -> case WAI.pathInfo $ fst requestInfo of
    []    -> (Error mempty, requestInfo) -- TODO: NoMorePathParams Error
    (h:t) -> case Web.parseUrlPieceMaybe h of
      Nothing -> (Error mempty, requestInfo) -- TODO: CouldntParseParameter Error
      Just v  -> (Ok v, (\(request, requestBody) -> (request { WAI.pathInfo = t }, requestBody)) requestInfo)
  QueryParam name -> undefined
  QueryFlag name -> undefined
  Header name -> undefined
  JSON -> undefined
  FormParam name -> undefined
  Form -> undefined
  File name -> undefined
  Raw -> undefined

  -- | Helpers
  Now -> (Ok requestInfo, requestInfo)
  Abort e -> (Error e, requestInfo)
  Is x extractor' -> case extract extractor' requestInfo of
    (Error e, requestInfo') -> (Error e, requestInfo')
    (Ok x', requestInfo') ->
      if x == x'
        then (Ok (), requestInfo')
        else (Error mempty, requestInfo')
        -- TODO: Add error tag for `Is`
  Satisfy p extractor' -> case extract extractor' requestInfo of
    (Error e, requestInfo') -> (Error e, requestInfo')
    (Ok x, requestInfo') ->
      if p x
        then (Ok (), requestInfo')
        else (Error mempty, requestInfo')
        -- TODO: Add error tag for `Satisfy`
{-
  Assert p ->
    if p requestInfo
      then (Ok (), requestInfo)
      else (Error mempty, requestInfo)
        -- TODO: Add error tag for `Satisfy`
-}
