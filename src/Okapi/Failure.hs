{-# LANGUAGE RankNTypes #-}

module Okapi.Internal.Failure where

import Okapi.Internal.Types

-- ERROR FUNCTIONS

skip :: forall a m. MonadOkapi m => m a
skip = Except.throwError Skip

throw :: forall a m. MonadOkapi m => Response -> m a
throw = Except.throwError . Error

-- | Execute the next parser even if the first one throws an Error failure
(<!>) :: MonadOkapi m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

guardThrow :: forall a m. MonadOkapi m => Response -> Bool -> m ()
guardThrow _ True = pure ()
guardThrow response False = throw response

optionalThrow :: MonadOkapi m => m a -> m (Maybe a)
optionalThrow parser = (Just <$> parser) <!> pure Nothing

optionThrow :: MonadOkapi m => a -> m a -> m a
optionThrow value parser = do
  mbValue <- optionalThrow parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'
