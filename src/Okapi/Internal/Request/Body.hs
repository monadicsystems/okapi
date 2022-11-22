{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Okapi.Internal.Request.Body
  ( State (..),
    gets,
    modify,
    look,
    Body (..),
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
import qualified Okapi.Internal.Error as Error
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web

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
