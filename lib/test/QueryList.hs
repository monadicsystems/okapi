{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Okapi.Codec ((=.))
import Okapi.Protocol.Request.Query
    (ArrayStyle (..), list, list', deepObject)
import Okapi.Protocol.Request.Query qualified as Q
import Okapi.Protocol.Request.Query.DeepObject qualified as DO
import System.Exit (exitFailure)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

data RGB = RGB { rR :: Int, rG :: Int, rB :: Int } deriving (Eq, Show)

main :: IO ()
main = do
    -- ── Exploded arrays: repeated key ─────────────────────────────────────────
    let tags = list @Text Exploded "tags"
    assertEq "exploded print"
        [("tags", Just "a"), ("tags", Just "b")]
        (Q.print tags ("a" :| ["b"]))
    assertEq "exploded round-trip"
        (Right ("a" :| ["b", "c"]))
        (fst (Q.parse tags (Q.print tags ("a" :| ["b", "c"]))))

    -- ── Comma / space / pipe delimited: one key, joined value ─────────────────
    let ids = list @Int CommaDelimited "ids"
    assertEq "comma print"
        [("ids", Just "1,2,3")]
        (Q.print ids (1 :| [2, 3]))
    assertEq "comma round-trip"
        (Right (1 :| [2, 3]))
        (fst (Q.parse ids (Q.print ids (1 :| [2, 3]))))

    let sp = list @Int SpaceDelimited "ids"
    assertEq "space print"  [("ids", Just "1 2 3")] (Q.print sp (1 :| [2, 3]))
    assertEq "space round-trip" (Right (1 :| [2, 3])) (fst (Q.parse sp (Q.print sp (1 :| [2, 3]))))

    let pp = list @Int PipeDelimited "ids"
    assertEq "pipe print"  [("ids", Just "1|2|3")] (Q.print pp (1 :| [2, 3]))
    assertEq "pipe round-trip" (Right (1 :| [2, 3])) (fst (Q.parse pp (Q.print pp (1 :| [2, 3]))))

    -- ── Required vs optional ──────────────────────────────────────────────────
    assertEq "required list rejects absent key"
        (Left Q.ParseError)
        (fst (Q.parse tags []))
    let tags' = list' @Text Exploded "tags"
    assertEq "optional list absent yields []"
        (Right [])
        (fst (Q.parse tags' []))
    assertEq "optional list round-trip"
        (Right ["a", "b"])
        (fst (Q.parse tags' (Q.print tags' ["a", "b"])))

    -- ── deepObject ────────────────────────────────────────────────────────────
    let rgb = deepObject "color"
                (RGB <$> (rR =. DO.field "R")
                     <*> (rG =. DO.field "G")
                     <*> (rB =. DO.field "B"))
    assertEq "deepObject print (bracketed keys)"
        [("color[R]", Just "1"), ("color[G]", Just "2"), ("color[B]", Just "3")]
        (Q.print rgb (RGB 1 2 3))
    assertEq "deepObject round-trip"
        (Right (RGB 10 20 30))
        (fst (Q.parse rgb (Q.print rgb (RGB 10 20 30))))
    -- a missing required field fails
    assertEq "deepObject rejects missing field"
        (Left Q.ParseError)
        (fst (Q.parse rgb [("color[R]", Just "1"), ("color[G]", Just "2")]))

    putStrLn "all query-list tests passed"
