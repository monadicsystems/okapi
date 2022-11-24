{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.HTTP.Request.Cookie
  ( Cookie,
    Crumb,
    parse,
    crumb,
    end,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Okapi.HTTP.Error as Error
import Okapi.Internal.Request.Headers
import qualified Okapi.HTTP.Request.Headers as Headers
import qualified Web.Cookie as Web

parse :: Headers.Parser m => m Cookie
parse = do
  cookieValue <- Headers.value "Cookie"
  pure $ Web.parseCookies cookieValue

crumb :: Headers.Parser m => BS.ByteString -> m BS.ByteString
crumb name = do
  cookieValue <- parse
  case List.lookup name cookieValue of
    Nothing -> Error.next
    Just crumbValue -> do
      let crumb = (name, crumbValue)
      -- TODO: Needs testing to see if state is restored properly
      modify (\headers -> ("Cookie", LBS.toStrict $ Builder.toLazyByteString $ Web.renderCookies $ List.delete crumb cookieValue) : headers)
      pure crumbValue

end :: Headers.Parser m => m ()
end = do
  currentCookie <- parse
  if List.null currentCookie
    then pure ()
    else Error.next
