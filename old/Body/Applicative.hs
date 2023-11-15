-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE OverloadedRecordDot #-}

-- module Okapi.Parser.Body.Applicative where

-- import Data.ByteString qualified as BS
-- import Data.List qualified as List
-- import Data.Text qualified as Text
-- import Data.Text.Encoding qualified as Text
-- import Network.HTTP.Types qualified as HTTP
-- import Web.HttpApiData qualified as Web
-- import Network.Wai qualified as Wai
-- import Network.Wai.Internal qualified as Wai
-- import Okapi.Parser.Headers.Operation qualified as Operation

-- data Parser a where
--   FMap :: (a -> b) -> Parser a -> Parser b
--   Pure :: a -> Parser a
--   Apply :: Parser (a -> b) -> Parser a -> Parser b
--   Optional :: Parser a -> Parser (Maybe a)
--   Option :: a -> Parser a -> Parser a
--   Operation :: Operation.Parser a -> Parser a

-- instance Functor Parser where
--   fmap = FMap

-- instance Applicative Parser where
--   pure = Pure
--   (<*>) = Apply

-- param :: Web.FromHttpApiData a => HTTP.HeaderName -> Parser a
-- param = Operation . Operation.Param

-- cookie :: BS.ByteString -> Parser ()
-- cookie = Operation . Operation.Cookie

-- optional :: Web.FromHttpApiData a => Parser a -> Parser (Maybe a)
-- optional = Optional

-- option :: Web.FromHttpApiData a => a -> Parser a -> Parser a
-- option = Option

-- eval ::
--   Parser a ->
--   Wai.Request ->
--   (Either Operation.Error a, Wai.Request)
-- eval (FMap f opX) state = case eval opX state of
--     (Left e, state') -> (Left e, state')
--     (Right x, state') -> (Right $ f x, state')
-- eval (Pure x) state = (Right x, state)
-- eval (Apply opF opX) state = case eval opF state of
--     (Right f, state') -> case eval opX state' of
--       (Right x, state'') -> (Right $ f x, state'')
--       (Left e, state'') -> (Left e, state'')
--     (Left e, state') -> (Left e, state')
-- eval (Optional op) state = case op of
--     Operation param@(Operation.Param _) -> case Operation.eval param state of
--       (Right result, state') -> (Right $ Just result, state')
--       (_, state') -> (Right Nothing, state')
--     Operation cookie@(Operation.Cookie _) -> case Operation.eval cookie state of
--       (Right result, state') -> (Right $ Just result, state')
--       (_, state') -> (Right Nothing, state')
--     _ -> case eval op state of
--       (Right result, state') -> (Right $ Just result, state')
--       (Left err, state') -> (Left err, state')
-- eval (Option def op) state = case op of
--     Operation param@(Operation.Param _) -> case Operation.eval param state of
--       (Right result, state') -> (Right result, state')
--       (_, state') -> (Right def, state')
--     Operation cookie@(Operation.Cookie _) -> case Operation.eval cookie state of
--       (Right result, state') -> (Right result, state')
--       (_, state') -> (Right def, state')
--     _ -> eval op state
-- eval (Operation op) state = Operation.eval op state

-- class FromQuery a where
--     parser :: Parser a

-- parse :: FromQuery a => Wai.Request -> Either Operation.Error a
-- parse req = fst $ eval parser req
