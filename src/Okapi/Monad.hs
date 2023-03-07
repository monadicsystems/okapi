{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Okapi.Monad where

import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wai.Parse as WAI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Web.Cookie as Web
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

data Next = Next

instance Semigroup Next where
  _ <> _ = Next

instance Monoid Next where
  mempty = Next
  mappend = (<>)

data RequestBody =
  RequestBodyRaw LBS.ByteString
  | RequestBodyMultipart ([WAI.Param], [WAI.File LBS.ByteString])
  deriving (Eq, Show)

type Parser a = Except.ExceptT Next (State.State (WAI.Request, RequestBody)) a

next :: Parser a
next = Except.throwError Next

method :: Parser BS.ByteString
method = do
  m <- State.gets (WAI.requestMethod . fst)
  State.modify (\(request, requestBody) -> (request { WAI.requestMethod = mempty }, requestBody))
  pure m

path :: Web.FromHttpApiData a => Parser [a]
path = Combinators.many pathParam

pathParam :: Web.FromHttpApiData a => Parser a
pathParam = do
  maybePathParam <- State.gets (safeHead . WAI.pathInfo . fst)
  case maybePathParam of
    Nothing -> next
    Just pathSeg -> do
      State.modify (\(request, requestBody) -> (request { WAI.pathInfo = drop 1 $ WAI.pathInfo $ request }, requestBody))
      maybe next pure (Web.parseUrlPieceMaybe pathSeg)
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x : _) = Just x

pathEnd :: Parser ()
pathEnd = path @Text.Text >>= \case
  [] -> pure ()
  _  -> next

query :: Parser [(BS.ByteString, Maybe BS.ByteString)]
query = do
  q <- State.gets (WAI.queryString . fst)
  State.modify (\(request, requestBody) -> (request { WAI.queryString = mempty }, requestBody))
  pure q

queryParam :: Web.FromHttpApiData a => BS.ByteString -> Parser a
queryParam name = do
  maybeQueryItem <- State.gets (Foldable.find (\(name', _) -> name == name') . WAI.queryString . fst)
  case maybeQueryItem of
    Nothing -> next
    Just queryItem@(_, maybeValueBS) -> case maybeValueBS of
      Nothing -> next
      Just valueBS -> case Web.parseQueryParamMaybe $ Text.decodeUtf8 valueBS of
        Nothing -> next
        Just value -> do
          State.modify (\(request, requestBody) -> (request { WAI.queryString = List.delete queryItem $ WAI.queryString $ request }, requestBody))
          pure value

queryFlag :: BS.ByteString -> Parser ()
queryFlag name = do
  maybeQueryItem <- State.gets (Foldable.find (\(name', _) -> name == name') . WAI.queryString . fst)
  case maybeQueryItem of
    Nothing -> next
    Just queryItem -> do
      State.modify (\(request, requestBody) -> (request { WAI.queryString = List.delete queryItem $ WAI.queryString $ request }, requestBody))
      pure ()

queryEnd :: Parser ()
queryEnd = query >>= \case
  [] -> pure ()
  _  -> next

body :: Parser RequestBody
body = do
  requestBody <- State.gets snd
  case requestBody of
    RequestBodyRaw raw -> do
      State.modify (\(request, requestBody) -> (request, RequestBodyRaw mempty))
      pure requestBody
    RequestBodyMultipart multipart -> do
      State.modify (\(request, requestBody) -> (request, RequestBodyMultipart mempty))
      pure requestBody

bodyJSON :: Aeson.FromJSON a => Parser a
bodyJSON = do
  b <- body
  case b of
    RequestBodyRaw lbs -> maybe next pure (Aeson.decode lbs)
    RequestBodyMultipart _ -> next

bodyURLEncoded :: _
bodyURLEncoded = undefined
