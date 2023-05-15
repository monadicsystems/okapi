{-# LANGUAGE GADTs #-}

module Okapi.Plug where

import Control.Applicative (Alternative (..))
import Data.Text
import qualified Network.Wai as WAI

data Conn = Conn {request :: WAI.Request, response :: WAI.Response}

data Error = Skipped [Text] | Halted [Text]

data Plug a where
  FMap :: (a -> b) -> Plug a -> Plug b
  Pure :: a -> Plug a
  Apply :: Plug (a -> b) -> Plug a -> Plug b
  Alt :: Plug a -> Plug a -> Plug a
  Skip :: [Text] -> Plug a
  Halt :: [Text] -> Plug a

instance Functor Plug where
  fmap = FMap

instance Applicative Plug where
  pure = Pure
  (<*>) = Apply

instance Alternative Plug where
  empty = Skip []
  (<|>) = Alt

skip = Skip

halt = Halt

class IsRoute r

class IsQuery q

class IsBody b

class IsHeaders h

class IsResponder r

class Monad m => IOable m

data Response = Response

data Endpoint where
  GET :: (IOable m, IsRoute r, IsQuery q, IsHeaders h, IsResponder res) => (r -> q -> h -> res -> m Response) -> Endpoint
  POST :: (IOable m, IsRoute r, IsQuery q, IsBody b, IsHeaders h, IsResponder res) => (r -> q -> h -> b -> res -> m Response) -> Endpoint
  PUT :: (IOable m, IsRoute r, IsQuery q, IsBody b, IsHeaders h, IsResponder res) => (r -> q -> h -> b -> res -> m Response) -> Endpoint
  DELETE :: (IOable m, IsRoute r, IsQuery q, IsHeaders h, IsResponder res) => (r -> q -> h -> res -> m Response) -> Endpoint

get :: (IOable m, IsRoute r, IsQuery q, IsHeaders h, IsResponder res) => (r -> q -> h -> res -> m Response) -> Endpoint
get = GET

post :: (IOable m, IsRoute r, IsQuery q, IsBody b, IsHeaders h, IsResponder res) => (r -> q -> h -> b -> res -> m Response) -> Endpoint
post = POST

put :: (IOable m, IsRoute r, IsQuery q, IsBody b, IsHeaders h, IsResponder res) => (r -> q -> h -> b -> res -> m Response) -> Endpoint
put = PUT

delete :: (IOable m, IsRoute r, IsQuery q, IsHeaders h, IsResponder res) => (r -> q -> h -> res -> m Response) -> Endpoint
delete = DELETE

router :: [Endpoint] -> Plug ()
router [] = pure ()
router (h : t) = undefined

-- Filter Endpoints by method
--

scope :: [Text] -> Plug () -> Plug ()
scope path plug = undefined

eval :: Plug a -> Conn -> (Either Error a, Conn)
eval = undefined
