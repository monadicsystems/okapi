{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Operator where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Okapi.Parser
import Okapi.Types
import qualified Web.HttpApiData as Web

-- | Parses and discards a single path segment matching the given @Text@ value
--
-- >>> parser = get // "store" // "clothing" >> respond ok
-- >>> result <- testParserIO parser (TestRequest "GET" [] "/store/clothing" "")
-- >>> assertResponse is200 result
-- True
(//) :: forall m a. MonadOkapi m => m a -> Text.Text -> m ()
(//) action goal = action >> pathSeg goal

(/:) :: forall m a. MonadOkapi m => m a -> Text.Text -> m ()
(/:) = undefined -- Adds named path param

(/*) :: forall m a. MonadOkapi m => m a -> m (NonEmpty.NonEmpty Text.Text)
(/*) action = action >> pathWildcard

-- Should be query flag?? YES
(??) :: forall m a. MonadOkapi m => m a -> Text.Text -> m ()
(??) action queryItemName =
  action >> do
    flag <- queryFlag queryItemName
    if flag then pure () else skip

(?:) :: forall m a. MonadOkapi m => m a -> Text.Text -> m ()
(?:) = undefined -- This should be the query param version of
