{-# LANGUAGE OverloadedStrings #-}

module Okapi.Protocol.Headers.Structured.Lexer (
    ParseError (..),
    Key,
    eParse,
    strip,
    stripLead,
    firstTop,
    splitTop,
    firstAndRest,
    breakKey,
    paramEntries,
    memberEntries,
    parseInnerBare,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

type Key = ByteString

data ParseError = ParseError deriving (Eq, Show)

eParse :: Either e a -> Either ParseError a
eParse = either (const (Left ParseError)) Right

paramEntries :: ByteString -> [(ByteString, Maybe ByteString)]
paramEntries = map breakKey . filter (not . BS.null) . map strip . splitTop 59

memberEntries :: ByteString -> [(ByteString, Maybe ByteString)]
memberEntries = map breakKey . filter (not . BS.null) . map strip . splitTop 44

breakKey :: ByteString -> (ByteString, Maybe ByteString)
breakKey bs = case firstTop 61 bs of
    Nothing -> (strip bs, Nothing)
    Just i  -> (strip (BS.take i bs), Just (strip (BS.drop (i + 1) bs)))

parseInnerBare :: ByteString -> Maybe [ByteString]
parseInnerBare m = case BS.uncons (strip m) of
    Just (40, rest) -> case firstTop 41 rest of
        Just i  -> Just (filter (not . BS.null) (map strip (splitTop 32 (BS.take i rest))))
        Nothing -> Nothing
    _ -> Nothing

firstTop :: Word8 -> ByteString -> Maybe Int
firstTop sep bs = go False (0 :: Int) 0
  where
    n = BS.length bs
    go inQ depth i
        | i >= n = Nothing
        | otherwise =
            let w = BS.index bs i
            in if w == 34 then go (not inQ) depth (i + 1)
               else if inQ then go inQ depth (i + 1)
               else if w == 40 then go inQ (depth + 1) (i + 1)
               else if w == 41 then go inQ (max 0 (depth - 1)) (i + 1)
               else if w == sep && depth == 0 then Just i
               else go inQ depth (i + 1)

splitTop :: Word8 -> ByteString -> [ByteString]
splitTop sep bs = case firstTop sep bs of
    Nothing -> [bs]
    Just i  -> BS.take i bs : splitTop sep (BS.drop (i + 1) bs)

firstAndRest :: Word8 -> ByteString -> (ByteString, ByteString)
firstAndRest sep bs = case firstTop sep bs of
    Nothing -> (bs, BS.empty)
    Just i  -> (BS.take i bs, BS.drop (i + 1) bs)

strip :: ByteString -> ByteString
strip = BS.dropWhileEnd isSp . BS.dropWhile isSp
  where isSp w = w == 32 || w == 9

stripLead :: ByteString -> ByteString
stripLead bs = fromMaybe bs (BS.stripPrefix ", " bs)
