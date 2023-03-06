{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Okapi.Style.Sinatra where

import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Okapi.Effect.Failure as Failure
import qualified Okapi.Effect.Request as Request
import qualified Okapi.Effect.Response as Response
import qualified Okapi.Effect.Server as Server
import qualified Okapi.Type.Server as Server
import qualified Web.HttpApiData as Web

-- class Request.MonadRequest m => SinatraRequestM m where
--   setParam :: Web.ToHttpApiData a => Text.Text -> a -> m ()

-- class Response.MonadResponse m => SinatraResponseM m where
--   param :: Web.FromHttpApiData a => Text.Text -> m a

-- class (SinatraRequestM m, SinatraResponseM m, Server.MonadHTTP m) => SinatraM m

-- instance SinatraRequestM m => SinatraRequestM (Server.ServerT m) where
--   setParam :: (Web.ToHttpApiData a, SinatraRequestM m) => Text.Text -> a -> Server.ServerT m ()
--   setParam name = Morph.lift . setParam name

-- instance SinatraResponseM m => SinatraResponseM (Server.ServerT m) where
--   param :: (Web.FromHttpApiData a, SinatraResponseM m) => Text.Text -> Server.ServerT m a
--   param = Morph.lift . param
{-
get ::
  (forall l. Request.MonadRequest l => l (Map.Map Text.Text Text.Text)) ->
  (forall m. Response.MonadResponse m => Map.Map Text.Text Text.Text -> m ()) ->
  (forall n. Server.MonadHTTP n => n ())
get setter responder = do
  Request.methodGET
  params <- setter
  responder params

post ::
  (forall l. Request.MonadRequest l => l (Map.Map Text.Text Text.Text)) ->
  (forall m. Response.MonadResponse m => Map.Map Text.Text Text.Text -> m ()) ->
  (forall n. Server.MonadHTTP n => n ())
post setter responder = do
  Request.methodPOST
  params <- setter
  responder params
-}

{-
get :: Server.MonadHTTP m => m a -> (a -> m ()) -> m ()
get handler = do
  Request.methodGET
  handler

post :: Server.MonadHTTP m => m a -> (a -> m ()) -> m ()
post handler = do
  Request.methodPOST
  handler

example :: Server.MonadHTTP m => m ()
example =
  Combinators.choice
    [ get [route|/greet/:Int|] \name -> do
        Response.setHeader ("Sinatra", "FromSinatra")
        Response.write @Text.Text "BlahBlah",
      post [route|/greet/:Text/:Int|] \(name, age) -> do
        Response.setHeader ("Sinatra", "PostResponse")
        Response.write age,
      put [route|/greet/:Int|] \age -> do
        Response.write age
    ]
-}