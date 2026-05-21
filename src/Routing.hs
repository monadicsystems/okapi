{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing where

import Control.Natural
import Data.Text
import GHC.Base
import GHC.Generics
import Network.Wai qualified as Wai

-- data Verb where
--     GET :: Verb
--     POST :: Verb

data Route a = Route a

data Props a = Props a

data Ctxt r p = Ctxt
    { route :: r
    , props :: p
    }

data Response = Response

data Method where
    GET ::
        forall r p m. Show p => 
        { route :: Route r
        , props :: Props p
        , handler :: Ctxt r p -> m Response
        , trans :: m ~> IO
        } ->
        Method
    POST ::
        forall r p m.
        { route :: Route r
        , props :: Props p
        , handler :: Ctxt r p -> m Response
        , trans :: m ~> IO
        } ->
        Method

app :: [Method]
app =
    [ GET
        { route = Route 5
        , props = Props "Bye"
        , trans = id
        , handler = \ctxt -> do
            return Response
        }
    , POST
        { route = Route "Bye"
        , props = Props 7
        , trans = id
        , handler = \ctxt -> do
            return Response
        }
    ]
