{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Failure
  ( module Okapi.Failure,
    module Okapi.Internal.Functions.Failure,
  )
where

import qualified Control.Monad.Except as Except
import Okapi.Internal.Functions.Failure
import Okapi.Internal.Types

-- | Execute the next parser even if the first one throws an Error failure
(<!>) :: forall a m. MonadOkapi m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardE :: forall a m. MonadOkapi m => Response -> Bool -> m ()
guardE _ True = pure ()
guardE response False = throw response

optionalE :: forall a m. MonadOkapi m => m a -> m (Maybe a)
optionalE parser = (Just <$> parser) <!> pure Nothing

optionE :: forall a m. MonadOkapi m => a -> m a -> m a
optionE value parser = do
  mbValue <- optionalE parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'
