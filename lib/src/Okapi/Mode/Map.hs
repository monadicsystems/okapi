{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Mode.Map (
    URI (..),
    Map (..),
    GMap,
    links,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
    ( D1, C1, S1, K1 (..), M1 (..), Rec0
    , Generic (..), Rep
    , (:*:) (..)
    )
import GHC.Records (HasField (..))
import Network.HTTP.Types qualified as HTTP
import Okapi.Mode.Forest (Forest (..), Shape)
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.Record.Tree qualified as Tree

data URI = URI
    { path  :: Text
    , query :: Text
    }

instance HasField "full" URI Text where
    getField uri = uri.path <> uri.query

data Map shape where
    Location ::
        (path -> query -> URI) ->
        Map (Shape method path query headers body result)

buildURI ::
    Tree.Request method path query headers body ->
    path ->
    query ->
    URI
buildURI req pathVal queryVal = URI
    { path  = "/" <> T.intercalate "/" (Path.printer req.path pathVal)
    , query = decodeUtf8 (HTTP.renderQuery True (Query.printer req.query queryVal))
    }

class GMap (epF :: Type -> Type) (lnF :: Type -> Type) where
    gMap :: epF () -> lnF ()

instance GMap epF lnF => GMap (D1 dm epF) (D1 dm' lnF) where
    gMap (M1 ep) = M1 (gMap @epF @lnF ep)

instance GMap epF lnF => GMap (C1 cm epF) (C1 cm' lnF) where
    gMap (M1 ep) = M1 (gMap @epF @lnF ep)

instance (GMap epL lnL, GMap epR lnR) => GMap (epL :*: epR) (lnL :*: lnR) where
    gMap (epL :*: epR) = gMap @epL @lnL epL :*: gMap @epR @lnR epR

instance GMap
    (S1 sm  (Rec0 (Forest (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Map (Shape method path query headers body result)))) where
    gMap (M1 (K1 ep)) = M1 (K1 (case ep of
        (req :-> _) -> Location (buildURI req)
        (req :-< _) -> Location (buildURI req)))

links ::
    forall server.
    ( Generic (server Forest)
    , Generic (server Map)
    , GMap (Rep (server Forest)) (Rep (server Map))
    ) =>
    server Forest ->
    server Map
links endpoints =
    to (gMap @(Rep (server Forest)) @(Rep (server Map)) (from endpoints))
