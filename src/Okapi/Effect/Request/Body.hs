{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Effect.Request.Body where

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Encoding as Text
import qualified Network.Wai.Parse as WAI
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.State.Request.Body as Body
import qualified Okapi.Type.Failure as Failure
import qualified Okapi.Type.Request as Request
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

class (Monad.MonadPlus m, Except.MonadError Failure.Failure m, Logger.MonadLogger m, Body.MonadState m) => MonadBody m

-- $bodyParsers

-- | For getting the raw body of the request.
body :: MonadBody m => m Request.Body
body = do
  currentBody <- Body.get
  case currentBody of
    Request.Raw (LBS.null -> True) -> pure currentBody
    Request.Multipart ([], []) -> pure currentBody
    Request.Raw _ -> do
      Body.put $ Request.Raw ""
      pure currentBody
    Request.Multipart _ -> do
      Body.put $ Request.Multipart ([], [])
      pure currentBody

-- | Parse request body as JSON
bodyJSON :: (Aeson.FromJSON a, MonadBody m) => m a
bodyJSON = do
  body' <- body
  case body' of
    Request.Raw lbs -> maybe Failure.next pure (Aeson.decode lbs)
    Request.Multipart _ -> Failure.next

-- | Parse URLEncoded form parameters from request body
bodyURLEncoded :: (Web.FromForm a, MonadBody m) => m a
bodyURLEncoded = do
  body' <- body
  case body' of
    Request.Raw lbs -> maybe Failure.next pure (eitherToMaybe $ Web.urlDecodeAsForm lbs)
    Request.Multipart _ -> Failure.next
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- | Parse multipart form data from request body
bodyMultipart :: MonadBody m => m ([WAI.Param], [WAI.File LBS.ByteString])
bodyMultipart = do
  body' <- body
  case body' of
    Request.Raw _ -> Failure.next
    Request.Multipart formData -> pure formData

-- | Parse a single form parameter
formParam :: forall a m. (Web.FromHttpApiData a, MonadBody m) => BS.ByteString -> m a
formParam paramName = do
  body' <- body
  case body' of
    Request.Raw lbs -> do
      case Web.urlDecodeParams lbs of
        Left _ -> Failure.next
        Right params ->
          case lookup (Text.decodeUtf8 paramName) params of
            Nothing -> Failure.next
            Just paramValue -> do
              paramValue' <- maybe Failure.next pure (Web.parseQueryParamMaybe paramValue)
              let newParams = List.delete (Text.decodeUtf8 paramName, paramValue) params
              Body.put $ Request.Raw $ Web.urlEncodeParams newParams
              pure paramValue'
    Request.Multipart (params, files) -> do
      case lookup paramName params of
        Nothing -> Failure.next
        Just paramValue -> do
          paramValue' <- maybe Failure.next pure (Web.parseQueryParamMaybe $ Text.decodeUtf8 paramValue)
          let newParams = List.delete (paramName, paramValue) params
          Body.put $ Request.Multipart (newParams, files)
          pure paramValue'

formParamList :: forall a m. (Web.FromHttpApiData a, MonadBody m) => BS.ByteString -> m (NonEmpty.NonEmpty a)
formParamList = Combinators.NonEmpty.some . formParam

-- | Parse a single form file
formFile :: MonadBody m => BS.ByteString -> m (WAI.FileInfo LBS.ByteString)
formFile paramName = do
  body' <- body
  case body' of
    Request.Raw _ -> Failure.next
    Request.Multipart (params, files) -> do
      case lookup paramName files of
        Nothing -> Failure.next
        Just fileInfo -> do
          let newFiles = deleteFile paramName files
          Body.put $ Request.Multipart (params, newFiles)
          pure fileInfo
  where
    deleteFile :: BS.ByteString -> [WAI.File LBS.ByteString] -> [WAI.File LBS.ByteString]
    deleteFile paramName [] = []
    deleteFile paramName (f@(paramName', fileInfo) : fs) =
      if paramName == paramName'
        then fs
        else f : deleteFile paramName fs

bodyEnd :: MonadBody m => m ()
bodyEnd = do
  currentBody <- body
  case currentBody of
    Request.Raw (LBS.null -> True) -> pure ()
    Request.Multipart ([], []) -> pure ()
    _ -> Failure.next

look :: MonadBody m => m a -> m a
look action = do
  request <- Body.get
  result <- action
  Body.put request
  pure result
