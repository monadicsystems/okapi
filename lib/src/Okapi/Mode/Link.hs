{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Okapi.Mode.Link (
    URI (..),
    Link (..),
    build,
    GLink,
    links,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (
    C1,
    D1,
    Generic (..),
    K1 (..),
    M1 (..),
    Rec0,
    Rep,
    S1,
    (:*:) (..),
 )
import GHC.Records (HasField (..))
import Network.HTTP.Types qualified as HTTP
import Okapi.HTTP.Request.Path qualified as Path
import Okapi.HTTP.Request.Query qualified as Query
import Okapi.Mode.Forest (Forest (..), Shape, stripTags)
import Okapi.Record.Tree qualified as Tree

data URI = URI
    { path :: Text
    , query :: Text
    }

instance HasField "full" URI Text where
    getField uri = uri.path <> uri.query

data Link shape where
    Builder ::
        (path -> query -> URI) ->
        Link (Shape method path query headers body result)

build ::
    Tree.Request method path query headers body ->
    path ->
    query ->
    URI
build req pathVal queryVal =
    URI
        { path = "/" <> T.intercalate "/" (Path.printer req.path pathVal)
        , query = decodeUtf8 (HTTP.renderQuery True (Query.printer req.query queryVal))
        }

class GLink (epF :: Type -> Type) (lnF :: Type -> Type) where
    gLink :: epF () -> lnF ()

instance (GLink epF lnF) => GLink (D1 dm epF) (D1 dm' lnF) where
    gLink (M1 ep) = M1 (gLink @epF @lnF ep)

instance (GLink epF lnF) => GLink (C1 cm epF) (C1 cm' lnF) where
    gLink (M1 ep) = M1 (gLink @epF @lnF ep)

instance (GLink epL lnL, GLink epR lnR) => GLink (epL :*: epR) (lnL :*: lnR) where
    gLink (epL :*: epR) = gLink @epL @lnL epL :*: gLink @epR @lnR epR

instance
    GLink
        (S1 sm (Rec0 (Forest (Shape method path query headers body result))))
        (S1 sm' (Rec0 (Link (Shape method path query headers body result))))
    where
    gLink (M1 (K1 ep)) =
        M1
            ( K1
                ( case stripTags ep of
                    (req :-> _) -> Builder (build req)
                    (req :-< _) -> Builder (build req)
                    Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"
                )
            )

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
