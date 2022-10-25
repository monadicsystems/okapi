module Conduit.Database where

import qualified Conduit.App as App
import qualified Control.Monad.Reader as Reader
import qualified Okapi
import qualified Hasql.Pool as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Control.Monad.IO.Class as IO
import qualified Text.InterpolatedString.Perl6 as Perl6
import qualified Data.Maybe as Maybe

runStatement :: (IO.MonadIO m, Okapi.ServerM m, Reader.MonadReader App.AppEnv m) => Hasql.Statement () o -> m o
runStatement statement = do
    let
        transaction = Hasql.statement () statement
        session = Hasql.transaction Hasql.Serializable Hasql.Write transaction
    dbPool <- App.grab @Hasql.Pool
    dbResult <- IO.liftIO $ Hasql.use dbPool session
    case dbResult of
      -- TODO: May lose session state here because cookies won't be sent back
      Left usageError -> Okapi.throw (Okapi.internalServerError { Okapi.responseBody = Okapi.ResponseBodyRaw [Perl6.qq| {usageError} |]})
      Right success -> pure success

runStatementOne :: (IO.MonadIO m, Okapi.ServerM m, Reader.MonadReader App.AppEnv m) => Hasql.Statement () [a] -> m (Maybe a)
runStatementOne statement = do
    rows <- runStatement statement
    pure $ Maybe.listToMaybe rows
