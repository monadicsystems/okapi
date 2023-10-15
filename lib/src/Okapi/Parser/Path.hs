{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Parser.Path where

import Control.Natural
import Data.Data (Typeable)
import Data.Function ((&))
import Data.Kind
import Data.Map
import Data.Text
import Data.Typeable
import GHC.Base (undefined)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Okapi.NewDSL
import Okapi.Route qualified as Route
import Web.HttpApiData qualified as Web

type Handler env = Wai.Request -> env Wai.Response

data API where
  Router    :: forall a env. (env ~> IO) -> Route.Route a -> (a -> Handler env) -> API
  Endpoint  :: forall a env. (env ~> IO) -> (HTTP.StdMethod -> Bool) -> Route.Route a -> (a -> Handler env) -> API
  MethodMap :: forall a env. (env ~> IO) -> Route.Route a -> Map HTTP.StdMethod (a -> Handler env) -> API
  Scope     :: Wai.Middleware -> [Text] -> [API] -> API
  DynScope  :: Wai.Middleware -> Route.Route a -> (a -> [API]) -> API

router :: forall a env. (env ~> IO) -> Route.Route a -> (a -> Handler env) -> API
router = Router

endpoint :: forall a env. (env ~> IO) -> (HTTP.StdMethod -> Bool) -> Route.Route a -> (a -> Handler env) -> API
endpoint = Endpoint @a @env

get_ :: Int -> Int -> Int
get_ = undefined

methodMap :: forall a env. (env ~> IO) -> Route.Route a -> Map HTTP.StdMethod (a -> Handler env) -> API
methodMap = MethodMap @a @env

scope :: Wai.Middleware -> [Text] -> [API] -> API
scope = Scope

dynScope :: Wai.Middleware -> Route.Route a -> (a -> [API]) -> API
dynScope = DynScope

helloWorld :: Route.Route (Text, Int)
helloWorld = do
  _ <- Route.static "hello"
  _ <- Route.static "world"
  name <- Route.param @Text
  age <- Route.param @Int
  pure (name, age)

helloWorld' :: Route.Route (Text, Int)
helloWorld' = do
  _ <- Route.static "helloz"
  _ <- Route.static "world"
  name <- Route.param @Text
  age <- Route.param @Int
  pure (name, age)

helloWorld'' :: Route.Route (Text, Int)
helloWorld'' = do
  _ <- Route.static "hello"
  _ <- Route.static "world"
  age <- Route.param @Int
  name <- Route.param @Text
  pure (name, age)

data Person = Person {name :: Text, age :: Int, salary :: Float}

xRoute :: Route.Route Int
xRoute = do
  _ <- Route.static "x"
  x <- Route.param @Int
  pure x

yRoute :: Route.Route Int
yRoute = do
  _ <- Route.static "y"
  y <- Route.param @Int
  pure y

zRoute :: Route.Route Int
zRoute = do
  _ <- Route.static "z"
  x <- xRoute
  y <- yRoute
  pure (x + y)

xyRoute :: Route.Route (Int, Int)
xyRoute = do
  _ <- Route.static "xy"
  x <- xRoute
  y <- yRoute
  pure (x, y)

data Datum = Datum {foo :: Int, bar :: Int, baz :: Int}

convert :: forall a. Typeable a => String
convert = show . typeRep $ Proxy @a
