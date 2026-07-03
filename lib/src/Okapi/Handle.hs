{-# LANGUAGE LambdaCase #-}

module Okapi.Handle
    ( Route (..)
    , Handle (..)
    , handle
    ) where

import Data.Kind (Type)
import Network.Wai qualified as Wai
import Okapi.Mode.Forest (Forest (..))
import Okapi.Mode.Server (Server (..), type (~>))
import Okapi.HTTP.Request qualified as Req
import Okapi.HTTP.Response qualified as Res
import Okapi.HTTP.Responses qualified as Resps

data Route (n :: Type -> Type) where
    (:=) :: Forest shape -> Server n shape -> Route n

infixl 0 :=

newtype Handle = Handle
    { runHandle :: Wai.Request -> IO (Maybe Wai.Response) }

handle :: (n ~> IO) -> Route n -> Handle
handle nt (forest := Function f) = Handle $ \waiReq -> case forest of
    (req :-> singleRes) ->
        Req.parseRequest req waiReq >>= \case
            Left _       -> pure Nothing
            Right reqVal -> do
                resVal <- nt (f (reqVal, waiReq))
                waiRes <- Res.printResponse singleRes resVal
                pure (Just waiRes)
    (req :-< resContracts) ->
        Req.parseRequest req waiReq >>= \case
            Left _       -> pure Nothing
            Right reqVal -> do
                resVal <- nt (f (reqVal, waiReq))
                waiRes <- Resps.printResponses resContracts resVal
                pure (Just waiRes)
