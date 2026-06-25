{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.ByteString (ByteString)
import Okapi.Codec (Codec, (=.))
import Okapi.Protocol.Shared.Headers.Fields (Fields, ParseError (..), dictionary, item, list, parseFields, printFields)
import Okapi.Protocol.Shared.Headers.Fields.Dictionary qualified as D
import Okapi.Protocol.Shared.Headers.Fields.Item qualified as I
import Okapi.Protocol.Shared.Headers.Fields.List qualified as L
import Okapi.Protocol.Shared.Headers.Fields.Parameters qualified as P
import System.Exit (exitFailure)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

-- An Item field: a token bare value (e.g. a media type) + an optional charset param.
itemCodec :: Codec Fields (ByteString, Maybe ByteString) (ByteString, Maybe ByteString)
itemCodec = item (I.item @ByteString (P.param' @ByteString "charset"))

-- A Dictionary field of integer-valued members.
dictCodec :: Codec Fields (Int, Int) (Int, Int)
dictCodec =
    dictionary
        ( (,) <$> (fst =. D.atItem @Int "a" I.bareItem)
              <*> (snd =. D.atItem @Int "b" I.bareItem)
        )

-- A homogeneous List field of tokens (Accept-Encoding style).
listCodec :: Codec Fields [ByteString] [ByteString]
listCodec = list (L.items (I.bareItem @ByteString))

main :: IO ()
main = do
    -- ── Item: bare token + optional param ────────────────────────────────────
    assertEq "item parse (with charset)"
        (Right ("application/json", Just "utf-8"))
        (fst (parseFields itemCodec "application/json; charset=utf-8"))
    assertEq "item parse (no param)"
        (Right ("application/json", Nothing))
        (fst (parseFields itemCodec "application/json"))
    assertEq "item print"
        "application/json;charset=utf-8"
        (printFields itemCodec ("application/json", Just "utf-8"))
    assertEq "item round-trip"
        (Right ("text/html", Just "utf-8"))
        (fst (parseFields itemCodec (printFields itemCodec ("text/html", Just "utf-8"))))

    -- ── Dictionary of integers ───────────────────────────────────────────────
    assertEq "dict parse"
        (Right (1, 2))
        (fst (parseFields dictCodec "a=1, b=2"))
    assertEq "dict parse (whitespace/order-independent)"
        (Right (1, 2))
        (fst (parseFields dictCodec "b=2 ,  a=1"))
    assertEq "dict print"
        "a=1, b=2"
        (printFields dictCodec (1, 2))
    assertEq "dict missing key fails"
        (Left ParseError)
        (fst (parseFields dictCodec "a=1"))

    -- ── List of tokens ───────────────────────────────────────────────────────
    assertEq "list parse"
        (Right ["gzip", "br", "deflate"])
        (fst (parseFields listCodec "gzip, br, deflate"))
    assertEq "list print"
        "gzip, br, deflate"
        (printFields listCodec ["gzip", "br", "deflate"])
    assertEq "list empty"
        (Right [])
        (fst (parseFields listCodec ""))

    putStrLn "all Fields tests passed"
