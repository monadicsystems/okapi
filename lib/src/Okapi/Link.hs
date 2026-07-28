{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.Link (
    URI (..),
    Link (..),
    build,
    GLink,
    links,
    GLinkVia,
    linksVia,
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
import Network.HTTP.Types qualified as Types
import Okapi.HTTP.Path qualified as Path
import Okapi.HTTP.Query qualified as Query
import Okapi.Contract (Contract (..), Signature, stripTags, Morph (..))
import Okapi.HTTP.Request qualified as Req

data URI = URI
    { path :: Text
    , query :: Text
    }

instance HasField "full" URI Text where
    getField uri = uri.path <> uri.query

data Link shape where
    Builder ::
        (path -> query -> URI) ->
        Link (Signature method path query headers body result)

build ::
    Req.Codec method path query headers body ->
    path ->
    query ->
    URI
build req pathVal queryVal =
    URI
        { path = "/" <> T.intercalate "/" (Path.printer req.path pathVal)
        , query = decodeUtf8 (Types.renderQuery True (Query.printer req.query queryVal))
        }

class GLink (ctF :: Type -> Type) (lnF :: Type -> Type) where
    gLink :: ctF () -> lnF ()

instance (GLink ctF lnF) => GLink (D1 dm ctF) (D1 dm' lnF) where
    gLink (M1 ct) = M1 (gLink @ctF @lnF ct)

instance (GLink ctF lnF) => GLink (C1 cm ctF) (C1 cm' lnF) where
    gLink (M1 ct) = M1 (gLink @ctF @lnF ct)

instance (GLink ctL lnL, GLink ctR lnR) => GLink (ctL :*: ctR) (lnL :*: lnR) where
    gLink (ctL :*: ctR) = gLink @ctL @lnL ctL :*: gLink @ctR @lnR ctR

instance
    GLink
        (S1 sm (Rec0 (Contract (Signature method path query headers body result))))
        (S1 sm' (Rec0 (Link (Signature method path query headers body result))))
    where
    gLink (M1 (K1 ct)) =
        M1
            ( K1
                ( case stripTags ct of
                    (req :-> _) -> Builder (build req)
                    (req :-< _) -> Builder (build req)
                    Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"
                )
            )

-- | Lets a field be a nested record of the same shape instead of a
--   concrete 'Contract'\/'Link' — recurses via 'links' itself. Same
--   non-overlap argument as the nested instances in "Okapi.Server".
instance
    ( Generic (nested Contract)
    , Generic (nested Link)
    , GLink (Rep (nested Contract)) (Rep (nested Link))
    ) =>
    GLink (S1 sm (Rec0 (nested Contract))) (S1 sm' (Rec0 (nested Link)))
    where
    gLink (M1 (K1 ctVal)) = M1 (K1 (links ctVal))

links ::
    forall record.
    ( Generic (record Contract)
    , Generic (record Link)
    , GLink (Rep (record Contract)) (Rep (record Link))
    ) =>
    record Contract ->
    record Link
links contracts =
    to (gLink @(Rep (record Contract)) @(Rep (record Link)) (from contracts))

class GLinkVia (ctF :: Type -> Type) (lnF :: Type -> Type) where
    gLinkVia :: ctF () -> lnF ()

instance (GLinkVia ctF lnF) => GLinkVia (D1 dm ctF) (D1 dm' lnF) where
    gLinkVia (M1 ct) = M1 (gLinkVia @ctF @lnF ct)

instance (GLinkVia ctF lnF) => GLinkVia (C1 cm ctF) (C1 cm' lnF) where
    gLinkVia (M1 ct) = M1 (gLinkVia @ctF @lnF ct)

instance (GLinkVia ctL lnL, GLinkVia ctR lnR) => GLinkVia (ctL :*: ctR) (lnL :*: lnR) where
    gLinkVia (ctL :*: ctR) = gLinkVia @ctL @lnL ctL :*: gLinkVia @ctR @lnR ctR

instance
    GLinkVia
        (S1 sm (Rec0 (Morph Contract n (Signature method path query headers body result))))
        (S1 sm' (Rec0 (Morph Link n (Signature method path query headers body result))))
    where
    gLinkVia (M1 (K1 (Morph ct))) =
        M1
            ( K1
                ( Morph
                    ( case stripTags ct of
                        (req :-> _) -> Builder (build req)
                        (req :-< _) -> Builder (build req)
                        Annotate _ _ -> error "unreachable: stripTags already peeled off every Annotate layer"
                    )
                )
            )

-- | Lets a field be a nested record of the same shape instead of a
--   concrete @Morph Contract@\/@Morph Link@ pair — recurses via
--   'linksVia' itself.
instance
    ( Generic (nested (Morph Contract))
    , Generic (nested (Morph Link))
    , GLinkVia (Rep (nested (Morph Contract))) (Rep (nested (Morph Link)))
    ) =>
    GLinkVia (S1 sm (Rec0 (nested (Morph Contract)))) (S1 sm' (Rec0 (nested (Morph Link))))
    where
    gLinkVia (M1 (K1 ctVal)) = M1 (K1 (linksVia ctVal))

{- | Heterogeneous-@n@ counterpart to 'links' — takes a record built with
  'Okapi.Contract.Morph' (see 'Okapi.Server.serversVia'). Output
  is @record (Morph Link)@, not plain @record Link@ — each field's @n@ is
  baked into the record's own field declarations, so the output has to stay
  2-arg-shaped to match; unwrap each field's 'Morph' to get the plain
  'Link' underneath, which is all link generation ever actually uses.
-}
linksVia ::
    forall record.
    ( Generic (record (Morph Contract))
    , Generic (record (Morph Link))
    , GLinkVia (Rep (record (Morph Contract))) (Rep (record (Morph Link)))
    ) =>
    record (Morph Contract) ->
    record (Morph Link)
linksVia contracts =
    to (gLinkVia @(Rep (record (Morph Contract))) @(Rep (record (Morph Link))) (from contracts))
