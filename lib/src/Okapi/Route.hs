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
import Okapi.Parser.Path qualified as Path
import Okapi.Parser.Query qualified as Query
import Okapi.Parser.Responder qualified as Responder
import Okapi.Parser.Responder.AddHeader (Response, toWaiResponse)
import Okapi.Parser.Security qualified as Security
import Okapi.Parser.Security.Secure qualified as Secure
import Okapi.Request (Request)

type Routes :: [Type] -> Type
data Routes resources where
  Nil :: Routes '[]
  (:&) :: Path.Interface resource => Route resource -> Routes resources -> Routes (resource ': resources)

infixr 5 :&

type Append :: forall a. [a] -> [a] -> [a] -- kind signature
type family Append xs ys where -- header
  Append '[] ys = ys -- clause 1
  Append (x ': xs) ys = x ': Append xs ys -- clause 2

appendRoutes :: Routes resources1 -> Routes resources2 -> Routes (Append resources1 resources2)
appendRoutes Nil pathItems = pathItems
appendRoutes (h :& t) pathItems = h :& appendRoutes t pathItems

data Route resource where
  Route ::
    Path.Interface resource =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      get :: Maybe (GET m resource security query headers responder),
      post :: Maybe (POST m resource security query body headers responder),
      put :: Maybe (PUT m resource security query body headers responder),
      delete :: Maybe (DELETE m resource security query headers responder)
    } ->
    Route resource

data GET m resource security query headers responder where
  GET ::
    (Monad m, Path.Interface resource, Security.Interface security, Query.Interface query, Headers.Interface headers, Responder.Interface responder) =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      object :: Object m,
      handler :: resource -> security -> query -> headers -> responder -> m Response
    } ->
    GET m resource security query headers responder

data POST m resource security query body headers responder where
  POST ::
    (Monad m, Path.Interface resource, Security.Interface security, Query.Interface query, Body.Interface body, Headers.Interface headers, Responder.Interface responder) =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      object :: Object m,
      handler :: resource -> security -> query -> body -> headers -> responder -> m Response
    } ->
    POST m resource security query body headers responder

data PUT m resource security query body headers responder where
  PUT ::
    (Monad m, Path.Interface resource, Security.Interface security, Query.Interface query, Body.Interface body, Headers.Interface headers, Responder.Interface responder) =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      object :: Object m,
      handler :: resource -> security -> query -> body -> headers -> responder -> m Response
    } ->
    PUT m resource security query body headers responder

data DELETE m resource security query headers responder where
  DELETE ::
    (Monad m, Path.Interface resource, Security.Interface security, Query.Interface query, Headers.Interface headers, Responder.Interface responder) =>
    { summary :: Maybe Text.Text,
      description :: Maybe Text.Text,
      object :: Object m,
      handler :: resource -> security -> query -> headers -> responder -> m Response
    } ->
    DELETE m resource security query headers responder
