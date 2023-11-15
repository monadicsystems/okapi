-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE OverloadedRecordDot #-}

-- module Okapi.Parser.Query.Operation where

-- import Data.ByteString qualified as BS
-- import Data.List qualified as List
-- import Data.Text qualified as Text
-- import Data.Text.Encoding qualified as Text
-- import Network.HTTP.Types qualified as HTTP
-- import Web.HttpApiData qualified as Web
-- import Network.Wai qualified as Wai
-- import Network.Wai.Internal qualified as Wai

-- data Error
--   = ParseFail
--   | FlagNotFound
--   | ParamNotFound
--   | ParamNoValue
--   deriving (Eq, Show)

-- data Parser a where
--   Param :: Web.FromHttpApiData a => BS.ByteString -> Parser a
--   Flag :: BS.ByteString -> Parser ()

-- eval ::
--   Parser a ->
--   Wai.Request ->
--   (Either Error a, Wai.Request)
-- eval (Param name) state = case lookup name state.queryString of
--     Nothing -> (Left ParamNotFound, state)
--     Just maybeVBS -> case maybeVBS of
--       Nothing -> (Left ParamNoValue, state)
--       Just vBS -> case Web.parseQueryParamMaybe $ Text.decodeUtf8 vBS of
--         Nothing -> (Left ParseFail, state)
--         Just v -> (Right v, state { Wai.queryString = List.delete (name, Just vBS) state.queryString })
-- eval (Flag name) state = case lookup name state.queryString of
--     Nothing -> (Left FlagNotFound, state)
--     Just found -> (Right (), state { Wai.queryString = List.delete (name, found) state.queryString })
