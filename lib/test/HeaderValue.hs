{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text)
import Okapi.Codec (Codec, (=.))
import Okapi.Protocol.Shared.Headers (ForRequest, Headers)
import Okapi.Protocol.Shared.Headers qualified as H
import Okapi.Protocol.Shared.Headers.Value qualified as HV
import System.Exit (exitFailure)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

-- a request-side structured Content-Type: assert the media type, capture optional charset
contentType :: Codec (Headers ForRequest) (Maybe Text) (Maybe Text)
contentType = H.structHeader "content-type" $
    (const () =. HV.flag "application/json") *> HV.param' "charset"

-- a comma-separated flat list header (Cache-Control)
cacheControl :: Codec (Headers ForRequest) (Maybe (), Maybe Int) (Maybe (), Maybe Int)
cacheControl = H.structHeaderL "cache-control" $
    (,) <$> (fst =. HV.flag' "no-cache") <*> (snd =. HV.param' @Int "max-age")

main :: IO ()
main = do
    -- ── Content-Type: media type asserted, charset captured/ignored ───────────
    -- print
    assertEq "content-type print (just media)"
        [("content-type", "application/json")]
        (H.print contentType Nothing)
    assertEq "content-type print (with charset)"
        [("content-type", "application/json; charset=utf-8")]
        (H.print contentType (Just "utf-8"))

    -- parse: tolerates the charset parameter (the whole point)
    assertEq "content-type parse tolerates ; charset"
        (Right (Just "utf-8"))
        (fst (H.parse contentType [("content-type", "application/json; charset=utf-8")]))
    assertEq "content-type parse bare media (no charset)"
        (Right Nothing)
        (fst (H.parse contentType [("content-type", "application/json")]))
    -- case-insensitive media type token
    assertEq "content-type parse is case-insensitive on token"
        (Right Nothing)
        (fst (H.parse contentType [("content-type", "Application/JSON")]))
    -- whitespace tolerance
    assertEq "content-type parse tolerates whitespace"
        (Right (Just "utf-8"))
        (fst (H.parse contentType [("content-type", "application/json ;  charset = utf-8")]))
    -- quoted parameter value
    assertEq "content-type parse unquotes value"
        (Right (Just "utf-8"))
        (fst (H.parse contentType [("content-type", "application/json; charset=\"utf-8\"")]))
    -- NEGATIVE: wrong media type fails (flag assertion)
    assertEq "content-type parse rejects wrong media type"
        (Left H.ParseError)
        (fst (H.parse contentType [("content-type", "text/html; charset=utf-8")]))
    -- NEGATIVE: absent header fails
    assertEq "content-type parse rejects absent header"
        (Left H.ParseError)
        (fst (H.parse contentType []))

    -- ── HSTS-style: param + optional flag, round-trip ─────────────────────────
    let hstsCodec = H.structHeader "strict-transport-security"
                      ((,) <$> (fst =. HV.param @Int "max-age") <*> (snd =. HV.flag' "includeSubDomains"))
            :: Codec (Headers ForRequest) (Int, Maybe ()) (Int, Maybe ())
    assertEq "hsts print"
        [("strict-transport-security", "max-age=31536000; includeSubDomains")]
        (H.print hstsCodec (31536000, Just ()))
    assertEq "hsts round-trip (with flag)"
        (Right (31536000, Just ()))
        (fst (H.parse hstsCodec (H.print hstsCodec (31536000, Just ()))))
    assertEq "hsts round-trip (no flag)"
        (Right (60, Nothing))
        (fst (H.parse hstsCodec (H.print hstsCodec (60, Nothing))))

    -- ── Cache-Control: comma-separated flat list ──────────────────────────────
    assertEq "cache-control comma print"
        [("cache-control", "no-cache, max-age=3600")]
        (H.print cacheControl (Just (), Just 3600))
    assertEq "cache-control comma round-trip"
        (Right (Just (), Just 3600))
        (fst (H.parse cacheControl (H.print cacheControl (Just (), Just 3600))))
    assertEq "cache-control parse partial (only max-age)"
        (Right (Nothing, Just 0))
        (fst (H.parse cacheControl [("cache-control", "max-age=0")]))
    -- comma is the separator, not semicolon: a ';' value stays one token
    assertEq "cache-control does not split on ;"
        (Right (Just (), Nothing))
        (fst (H.parse cacheControl [("cache-control", "no-cache")]))

    putStrLn "all header-value tests passed"
