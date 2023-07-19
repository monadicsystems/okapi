{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module API.Login where

import App (Context)
import Control.Object (Object)
import qualified Data
import Data.List.NonEmpty (NonEmpty ((:|)))
import Okapi.Operation (GET (..), POST (..), PathItem (..))
import qualified Okapi.Spec.Request.Body as Body
import qualified Okapi.Spec.Request.Headers as Headers
import qualified Okapi.Spec.Request.Query as Query
import qualified Okapi.Spec.Response as Response
import qualified Okapi.Spec.Response.Headers as Headers
import qualified Okapi.Spec.Request.Security as Security
import qualified Resource

toLogin :: Object Context -> PathItem Resource.Login
toLogin object =
  PathItem
    { summary = Nothing,
      description = Nothing,
      get = Nothing,
      post = Just $ postOperation object,
      put = Nothing,
      delete = Nothing
    }

postOperation :: Object Context -> POST Context Resource.Login Security Query Data.UserLogin Headers Response
postOperation object =
  POST
    { summary = Nothing,
      description = Nothing,
      handler = postHandler,
      object = object
    }

newtype Security = Security ()

instance Security.Interface Security where
  security :: NonEmpty (Security.Spec Security)
  security = (Security <$> Security.None) :| []

newtype Query = Query ()

instance Query.Interface Query where
  query = pure (Query ())

newtype Headers = Headers ()

instance Headers.Interface Headers where
  headers = pure (Headers ())

newtype Response = Response ()

instance Response.Interface Response where
  responder :: Response.Spec Response
  responder = pure (Response ())

postHandler :: Resource.Login -> Security -> Query -> Data.UserLogin -> Headers -> Response -> Context Headers.Response
postHandler = pure undefined
