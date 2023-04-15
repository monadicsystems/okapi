{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Endpoint.Query where

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Web.HttpApiData as Web

data Error
  = ParseFail
  | FlagNotFound
  | ParamNotFound
  | ParamNoValue

data Query a where
  FMap :: (a -> b) -> Query a -> Query b
  Pure :: a -> Query a
  Apply :: Query (a -> b) -> Query a -> Query b
  Param :: Web.FromHttpApiData a => BS.ByteString -> Query a
  Flag :: BS.ByteString -> Query ()

instance Functor Query where
  fmap = FMap

instance Applicative Query where
  pure = Pure
  (<*>) = Apply

run ::
  Query a ->
  HTTP.Query ->
  (Either Error a, HTTP.Query)
run op state = case op of
  FMap f opX ->
    case run opX state of
      (Left e, state') -> (Left e, state')
      (Right x, state') -> (Right $ f x, state')
  Pure x -> (Right x, state)
  Apply opF opX -> case run opF state of
    (Right f, state') -> case run opX state' of
      (Right x, state'') -> (Right $ f x, state'')
      (Left e, state'') -> (Left e, state'')
    (Left e, state') -> (Left e, state')
  Param name -> case lookup name state of
    Nothing -> (Left ParamNotFound, state)
    Just maybeVBS -> case maybeVBS of
      Nothing -> (Left ParamNoValue, state)
      Just vBS -> case Web.parseQueryParamMaybe $ Text.decodeUtf8 vBS of
        Nothing -> (Left ParseFail, state)
        Just v -> (Right v, List.delete (name, Just vBS) state)
  Flag name -> case lookup name state of
    Nothing -> (Left FlagNotFound, state)
    Just found -> (Right (), List.delete (name, found) state)
