{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Request.Body
  ( MonadBody (..),
    State (..),
    gets,
    modify,
    look,
    Body (..),
    body,
    bodyJSON,
    bodyURLEncoded,
    bodyMultipart,
    formParam,
    formParamList,
    formFile,
    bodyEnd,
  )
where

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
import qualified Okapi.Error as Error
import qualified Okapi.Log as Log
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

type MonadBody m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

class Monad m => State m where
  get :: m Body
  put :: Body -> m ()

gets :: State m => (Body -> a) -> m a
gets projection = projection <$> get

modify :: State m => (Body -> Body) -> m ()
modify modifier = do
  request <- get
  put $ modifier request

look :: State m => m a -> m a
look action = do
  request <- get
  result <- action
  put request
  pure result

data Body
  = Raw LBS.ByteString
  | Multipart ([WAI.Param], [WAI.File LBS.ByteString])
  deriving (Eq, Show)

-- $bodyParsers

-- | For getting the raw body of the request.
body :: MonadBody m => m Body
body = do
  currentBody <- get
  case currentBody of
    Raw (LBS.null -> True) -> pure currentBody
    Multipart ([], []) -> pure currentBody
    Raw _ -> do
      put $ Raw ""
      pure currentBody
    Multipart _ -> do
      put $ Multipart ([], [])
      pure currentBody

-- | Parse request body as JSON
bodyJSON :: (Aeson.FromJSON a, MonadBody m) => m a
bodyJSON = do
  body' <- body
  case body' of
    Raw lbs -> maybe Error.next pure (Aeson.decode lbs)
    Multipart _ -> Error.next

-- | Parse URLEncoded form parameters from request body
bodyURLEncoded :: (Web.FromForm a, MonadBody m) => m a
bodyURLEncoded = do
  body' <- body
  case body' of
    Raw lbs -> maybe Error.next pure (eitherToMaybe $ Web.urlDecodeAsForm lbs)
    Multipart _ -> Error.next
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
    Raw _ -> Error.next
    Multipart formData -> pure formData

-- | Parse a single form parameter
formParam :: forall a m. (Web.FromHttpApiData a, MonadBody m) => BS.ByteString -> m a
formParam paramName = do
  Log.logIt "Getting form param"
  body' <- body
  case body' of
    Raw lbs -> do
      case Web.urlDecodeParams lbs of
        Left _ -> Error.next
        Right params ->
          case lookup (Text.decodeUtf8 paramName) params of
            Nothing -> Error.next
            Just paramValue -> do
              paramValue' <- maybe Error.next pure (Web.parseQueryParamMaybe paramValue)
              let newParams = List.delete (Text.decodeUtf8 paramName, paramValue) params
              put $ Raw $ Web.urlEncodeParams newParams
              Log.logIt "Got form param"
              pure paramValue'
    Multipart (params, files) -> do
      case lookup paramName params of
        Nothing -> Error.next
        Just paramValue -> do
          paramValue' <- maybe Error.next pure (Web.parseQueryParamMaybe $ Text.decodeUtf8 paramValue)
          let newParams = List.delete (paramName, paramValue) params
          put $ Multipart (newParams, files)
          Log.logIt "Got form param"
          pure paramValue'

formParamList :: forall a m. (Web.FromHttpApiData a, MonadBody m) => BS.ByteString -> m (NonEmpty.NonEmpty a)
formParamList = Combinators.NonEmpty.some . formParam

-- | Parse a single form file
formFile :: MonadBody m => BS.ByteString -> m (WAI.FileInfo LBS.ByteString)
formFile paramName = do
  body' <- body
  case body' of
    Raw _ -> Error.next
    Multipart (params, files) -> do
      case lookup paramName files of
        Nothing -> Error.next
        Just fileInfo -> do
          let newFiles = deleteFile paramName files
          put $ Multipart (params, newFiles)
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
    Raw (LBS.null -> True) -> pure ()
    Multipart ([], []) -> pure ()
    _ -> Error.next
