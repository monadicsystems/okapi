{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module API.Login where

import App (Context)
import Control.Object (Object)
import qualified Data
import Data.List.NonEmpty (NonEmpty ((:|)))
import Okapi.Operation (GET (..), POST (..), PathItem (..))
import qualified Okapi.Parser.Body as Body
import qualified Okapi.Parser.Headers as Headers
import qualified Okapi.Parser.Query as Query
import qualified Okapi.Parser.Responder as Responder
import qualified Okapi.Parser.Responder.AddHeader as AddHeader
import qualified Okapi.Parser.Security as Security
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

postOperation :: Object Context -> POST Context Resource.Login Security Query Data.UserLogin Headers Responder
postOperation object =
  POST
    { summary = Nothing,
      description = Nothing,
      handler = postHandler,
      object = object
    }

newtype Security = Security ()

instance Security.Interface Security where
  security :: NonEmpty (Security.Parser Security)
  security = (Security <$> Security.None) :| []

newtype Query = Query ()

instance Query.Interface Query where
  query = pure (Query ())

newtype Headers = Headers ()

instance Headers.Interface Headers where
  headers = pure (Headers ())

newtype Responder = Responder ()

instance Responder.Interface Responder where
  responder :: Responder.Parser Responder
  responder = pure (Responder ())

postHandler :: Resource.Login -> Security -> Query -> Data.UserLogin -> Headers -> Responder -> Context AddHeader.Response
postHandler = pure undefined
