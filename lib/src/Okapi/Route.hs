{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi.Route where

import Control.Natural (type (~>))
import Control.Object (Object (..), (#))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Kind (Type)
import Data.List (groupBy)
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.OpenApi (OpenApi (_openApiInfo))
import Data.OpenApi qualified as OAPI
import Data.OpenApi.Declare qualified as OAPI
import Data.OpenApi.Internal (OpenApiSpecVersion (..), upperOpenApiSpecVersion)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Version qualified as Version
import Debug.Trace qualified as Debug
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as WAI
import Network.Wai.Parse qualified as WAI
import Okapi.Parser
import Okapi.Parser.Body qualified as Body
import Okapi.Parser.Headers qualified as Headers
import Okapi.Parser.Query qualified as Query
import Okapi.Parser.Responder qualified as Responder
import Okapi.Parser.Responder.AddHeader (Response, toWaiResponse)
import Okapi.Parser.Route qualified as Route
import Okapi.Parser.Security qualified as Security
import Okapi.Request (Request)

type Routes :: [Type] -> Type
data Routes routes where
  Nil :: Routes '[]
  (:&) :: Route.Interface route => Route route -> Routes routes -> Routes (route ': routes)

infixr 5 :&

type Append :: forall a. [a] -> [a] -> [a] -- kind signature
type family Append xs ys where -- header
  Append '[] ys = ys -- clause 1
  Append (x ': xs) ys = x ': Append xs ys -- clause 2

appendRoutes :: Routes resources1 -> Routes resources2 -> Routes (Append resources1 resources2)
appendRoutes Nil pathItems = pathItems
appendRoutes (h :& t) pathItems = h :& appendRoutes t pathItems

data Route route where
  Route ::
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      get :: Maybe (GET security route query headers responder),
      post :: Maybe (POST security route query body headers responder),
      put :: Maybe (PUT security route query body headers responder),
      delete :: Maybe (DELETE security route query headers responder)
    } ->
    Route route

data GET m security route query headers responder where
  GET ::
    IOable m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      handler :: GETParams security route query headers responder -> m Response
    } ->
    GET m security route query headers responder

data GETParams security route query headers responder where
  GETParams ::
    (Security.Interface security, Route.Interface route, Query.Interface query, Headers.Interface headers, Responder.Interface responder) =>
    { security :: security,
      route :: route,
      query :: query,
      headers :: headers,
      responder :: responder
    }

data POST m security route query body headers responder where
  POST ::
    IOable m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      handler :: POSTParams security route query headers responder -> m Response
    } ->
    POST m security route query body headers responder

data POSTParams security route query body headers responder where
  POSTParams ::
    (Security.Interface security, Route.Interface route, Query.Interface query, Body.Interface body, Headers.Interface headers, Responder.Interface responder) =>
    { security :: security,
      route :: route,
      query :: query,
      body :: body,
      headers :: headers,
      responder :: responder
    }

data PUT m security route query body headers responder where
  PUT ::
    IOable m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      handler :: PUTParams security route query headers responder -> m Response
    } ->
    PUT m security route query body headers responder

data PUTParams security route query body headers responder where
  PUTParams ::
    (Security.Interface security, Route.Interface route, Query.Interface query, Body.Interface body, Headers.Interface headers, Responder.Interface responder) =>
    { security :: security,
      route :: route,
      query :: query,
      body :: body,
      headers :: headers,
      responder :: responder
    }

data DELETE m security route query headers responder where
  DELETE ::
    IOable m =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      handler :: DELETEParams security route query headers responder -> m Response
    } ->
    DELETE m security route query headers responder

data DELETEParams security route query headers responder where
  DELETEParams ::
    (Security.Interface security, Route.Interface route, Query.Interface query, Headers.Interface headers, Responder.Interface responder) =>
    { security :: security,
      route :: route,
      query :: query,
      headers :: headers,
      responder :: responder
    }
