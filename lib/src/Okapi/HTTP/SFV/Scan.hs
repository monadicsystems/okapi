
-- | Shared delimiter-scanning helpers for the Structured structured-field-value
--   codecs (@Bare@, @Item@, @Parameters@, @List@, @Dictionary@), all of
--   which need to find top-level separators (@;@, @=@, @,@, @(@\/@)@) in a
--   single 'ByteString' while correctly skipping over separators that
--   appear inside a quoted @sf-string@ or a parenthesized inner list.
module Okapi.HTTP.SFV.Scan (
    strip,
    firstTop,
    firstAndRest,
    firstAndTail,
    splitTop,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)

isSp :: Word8 -> Bool
isSp w = w == 32 || w == 9

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp

-- | Find the index of the first top-level (not inside quotes, not inside
--   parentheses) occurrence of @sep@. Correctly skips backslash-escaped
--   bytes inside quotes (@\\"@, @\\\\@ per 'Okapi.HTTP.SFV.Bare'\'s
--   @escapeSfString@) — an escaped quote must not toggle quote-state, or a
--   real separator immediately after would be wrongly treated as still
--   inside the string.
--
--   The @sep == depth == 0@ check must come before the generic paren-depth
--   tracking: when @sep@ is itself @(@ or @)@ (as when hunting for an
--   inner list's closing paren), the depth-adjusting branches would
--   otherwise always intercept it first and it could never be reported as
--   the found separator.
firstTop :: Word8 -> ByteString -> Maybe Int
firstTop sep bs = go False (0 :: Int) 0
  where
    n = BS.length bs
    go inQ depth i
        | i >= n = Nothing
        | otherwise =
            let w = BS.index bs i
            in if inQ && w == 92 && i + 1 < n then go inQ depth (i + 2)
               else if w == 34 then go (not inQ) depth (i + 1)
               else if inQ then go inQ depth (i + 1)
               else if w == sep && depth == 0 then Just i
               else if w == 40 then go inQ (depth + 1) (i + 1)
               else if w == 41 then go inQ (max 0 (depth - 1)) (i + 1)
               else go inQ depth (i + 1)

firstAndRest :: Word8 -> ByteString -> (ByteString, ByteString)
firstAndRest sep bs = case firstTop sep bs of
    Nothing -> (bs, BS.empty)
    Just i  -> (BS.take i bs, BS.drop (i + 1) bs)

-- | Like 'firstAndRest', but keeps the separator itself attached to the
--   second half instead of dropping it — needed wherever the leftover must
--   be losslessly reconstructible via @printed <> leftover@.
firstAndTail :: Word8 -> ByteString -> (ByteString, ByteString)
firstAndTail sep bs = case firstTop sep bs of
    Nothing -> (bs, BS.empty)
    Just i  -> (BS.take i bs, BS.drop i bs)

splitTop :: Word8 -> ByteString -> [ByteString]
splitTop sep bs = case firstTop sep bs of
    Nothing -> [bs]
    Just i  -> BS.take i bs : splitTop sep (BS.drop (i + 1) bs)
