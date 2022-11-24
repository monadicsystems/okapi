{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.HTTP.Request.Body
  ( Parser (..),
    Body (..),
    parse,
    json,
    urlEncoded,
    multipart,
    formParam,
    formParams,
    file,
    end,
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
import qualified Okapi.HTTP.Error as Error
import qualified Okapi.Internal.Error as Error
import Okapi.Internal.Request.Body
import qualified Okapi.Log as Log
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

type Parser m = (Monad.MonadPlus m, Except.MonadError Error.Error m, Logger.MonadLogger m, State m)

-- \$bodyParsers

-- | For getting the raw parse of the request.
parse :: Parser m => m Body
parse = do
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

-- | Parse request parse as JSON
json :: (Aeson.FromJSON a, Parser m) => m a
json = do
  parse' <- parse
  case parse' of
    Raw lbs -> maybe Error.next pure (Aeson.decode lbs)
    Multipart _ -> Error.next

-- | Parse URLEncoded form parameters from request parse
urlEncoded :: (Web.FromForm a, Parser m) => m a
urlEncoded = do
  parse' <- parse
  case parse' of
    Raw lbs -> maybe Error.next pure (eitherToMaybe $ Web.urlDecodeAsForm lbs)
    Multipart _ -> Error.next
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- | Parse multipart form data from request parse
multipart :: Parser m => m ([WAI.Param], [WAI.File LBS.ByteString])
multipart = do
  parse' <- parse
  case parse' of
    Raw _ -> Error.next
    Multipart formData -> pure formData

-- | Parse a single form parameter
formParam :: forall a m. (Web.FromHttpApiData a, Parser m) => BS.ByteString -> m a
formParam paramName = do
  Log.logIt "Getting form formParam"
  parse' <- parse
  case parse' of
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
              Log.logIt "Got form formParam"
              pure paramValue'
    Multipart (params, files) -> do
      case lookup paramName params of
        Nothing -> Error.next
        Just paramValue -> do
          paramValue' <- maybe Error.next pure (Web.parseQueryParamMaybe $ Text.decodeUtf8 paramValue)
          let newParams = List.delete (paramName, paramValue) params
          put $ Multipart (newParams, files)
          Log.logIt "Got form formParam"
          pure paramValue'

formParams :: forall a m. (Web.FromHttpApiData a, Parser m) => BS.ByteString -> m (NonEmpty.NonEmpty a)
formParams = Combinators.NonEmpty.some . formParam

-- | Parse a single form file
file :: Parser m => BS.ByteString -> m (WAI.FileInfo LBS.ByteString)
file paramName = do
  parse' <- parse
  case parse' of
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

end :: Parser m => m ()
end = do
  currentBody <- parse
  case currentBody of
    Raw (LBS.null -> True) -> pure ()
    Multipart ([], []) -> pure ()
    _ -> Error.next
