{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Mode.Link (
    URI (..),
    Link (..),
    GLink,
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
    getField u = u.path <> u.query

data Link shape where
    Location ::
        (path -> query -> URI) ->
        Link (Shape method path query headers body result)

buildURI ::
    Tree.Request method path query headers body ->
    path ->
    query ->
    URI
buildURI req pathVal queryVal = URI
    { path  = "/" <> T.intercalate "/" (Path.printer req.path pathVal)
    , query = decodeUtf8 (HTTP.renderQuery True (Query.printer req.query queryVal))
    }

class GLink (epF :: Type -> Type) (lnF :: Type -> Type) where
    gLink :: epF () -> lnF ()

instance GLink epF lnF => GLink (D1 dm epF) (D1 dm' lnF) where
    gLink (M1 ep) = M1 (gLink @epF @lnF ep)

instance GLink epF lnF => GLink (C1 cm epF) (C1 cm' lnF) where
    gLink (M1 ep) = M1 (gLink @epF @lnF ep)

instance (GLink epL lnL, GLink epR lnR) => GLink (epL :*: epR) (lnL :*: lnR) where
    gLink (epL :*: epR) = gLink @epL @lnL epL :*: gLink @epR @lnR epR

instance GLink
    (S1 sm  (Rec0 (Forest (Shape method path query headers body result))))
    (S1 sm' (Rec0 (Link (Shape method path query headers body result)))) where
    gLink (M1 (K1 ep)) = M1 (K1 (case ep of
        (req :-> _) -> Location (buildURI req)
        (req :-< _) -> Location (buildURI req)))

links ::
    forall server.
    ( Generic (server Forest)
    , Generic (server Link)
    , GLink (Rep (server Forest)) (Rep (server Link))
    ) =>
    server Forest ->
    server Link
links endpoints =
    to (gLink @(Rep (server Forest)) @(Rep (server Link)) (from endpoints))
