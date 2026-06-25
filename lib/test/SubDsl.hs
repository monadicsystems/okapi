{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Okapi.Codec (Codec)
import Okapi.Protocol.Shared.Headers (ForRequest)
import Okapi.Protocol.Shared.Headers qualified as Headers
import Okapi.Protocol.Request.Path qualified as Path
import Okapi.Protocol.Request.Query qualified as Query
import System.Exit (exitFailure)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

main :: IO ()
main = do
    -- ── Path: seg_ ───────────────────────────────────────────────────────────
    assertEq "seg_ matches literal"
        (Right ())
        (Path.parseExact (Path.seg_ ("users" :: Text)) ["users"])

    assertEq "seg_ rejects wrong literal"
        (Left (Path.ParseError, ["posts"]))
        (Path.parseExact (Path.seg_ ("users" :: Text)) ["posts"])

    assertEq "seg_ rejects excess segments"
        (Left (Path.ParseError, ["extra"]))
        (Path.parseExact (Path.seg_ ("users" :: Text)) ["users", "extra"])

    -- ── Path: seg ────────────────────────────────────────────────────────────
    let segInt = Path.seg @Int "id"
    assertEq "seg roundtrip"
        (Right 42)
        (Path.parseExact segInt (Path.print segInt 42))

    assertEq "seg rejects unparseable input"
        (Left (Path.ParseError, ["abc"]))
        (Path.parseExact segInt ["abc"])

    -- ── Path: segs ───────────────────────────────────────────────────────────
    let segsText = Path.segs @Text
    assertEq "segs roundtrip"
        (Right ("a" :| ["b", "c"]))
        (Path.parseExact segsText (Path.print segsText ("a" :| ["b", "c"])))

    assertEq "segs rejects empty"
        (Left (Path.ParseError, []))
        (Path.parseExact segsText [])

    -- ── Path: composition ────────────────────────────────────────────────────
    let usersId = Path.seg_ ("users" :: Text) *> Path.seg @Int "id"
    assertEq "seg_ *> seg roundtrip"
        (Right 7)
        (Path.parseExact usersId (Path.print usersId 7))

    assertEq "seg_ *> seg rejects excess"
        (Left (Path.ParseError, ["extra"]))
        (Path.parseExact usersId (Path.print usersId 7 ++ ["extra"]))

    -- ── Query: param ─────────────────────────────────────────────────────────
    let qCount = Query.param @Int "count"
    assertEq "param roundtrip"
        (Right 5)
        (fst (Query.parse qCount (Query.print qCount 5)))

    assertEq "param missing fails"
        (Left Query.ParseError)
        (fst (Query.parse qCount []))

    -- ── Query: param' ────────────────────────────────────────────────────────
    let qCountOpt = Query.param' @Int "count"
    assertEq "param' Nothing roundtrip"
        (Right Nothing)
        (fst (Query.parse qCountOpt (Query.print qCountOpt Nothing)))

    assertEq "param' Just roundtrip"
        (Right (Just 5))
        (fst (Query.parse qCountOpt (Query.print qCountOpt (Just 5))))

    assertEq "param' absent yields Nothing"
        (Right Nothing)
        (fst (Query.parse qCountOpt []))

    -- ── Query: flag ──────────────────────────────────────────────────────────
    let qDebug = Query.flag "debug"
    assertEq "flag roundtrip"
        (Right ())
        (fst (Query.parse qDebug (Query.print qDebug ())))

    assertEq "flag absent fails"
        (Left Query.ParseError)
        (fst (Query.parse qDebug []))

    -- ── Query: flag' ─────────────────────────────────────────────────────────
    let qVerbose = Query.flag' "verbose"
    assertEq "flag' Just () roundtrip"
        (Right (Just ()))
        (fst (Query.parse qVerbose (Query.print qVerbose (Just ()))))

    assertEq "flag' Nothing roundtrip"
        (Right Nothing)
        (fst (Query.parse qVerbose (Query.print qVerbose Nothing)))

    -- ── Query: extra params are lenient ──────────────────────────────────────
    let qExtra = [("other", Just "val")]
    assertEq "extra query params ignored (param)"
        (Right 5)
        (fst (Query.parse qCount (Query.print qCount 5 ++ qExtra)))

    assertEq "extra query params ignored (flag)"
        (Right ())
        (fst (Query.parse qDebug (Query.print qDebug () ++ qExtra)))

    -- ── Headers: header ──────────────────────────────────────────────────────
    let hCount = Headers.header "x-count" :: Codec (Headers.Headers ForRequest) Int Int
    assertEq "header roundtrip"
        (Right 10)
        (fst (Headers.parse hCount (Headers.print hCount 10)))

    assertEq "header missing fails"
        (Left Headers.ParseError)
        (fst (Headers.parse hCount []))

    -- ── Headers: header' ─────────────────────────────────────────────────────
    let hCountOpt = Headers.header' "x-count" :: Codec (Headers.Headers ForRequest) (Maybe Int) (Maybe Int)
    assertEq "header' Nothing roundtrip"
        (Right Nothing)
        (fst (Headers.parse hCountOpt (Headers.print hCountOpt Nothing)))

    assertEq "header' Just roundtrip"
        (Right (Just 10))
        (fst (Headers.parse hCountOpt (Headers.print hCountOpt (Just 10))))

    assertEq "header' absent yields Nothing"
        (Right Nothing)
        (fst (Headers.parse hCountOpt []))

    -- ── Headers: extra headers are lenient ───────────────────────────────────
    let hExtra = [("x-other", "value")]
    assertEq "extra headers ignored"
        (Right 10)
        (fst (Headers.parse hCount (Headers.print hCount 10 ++ hExtra)))

    -- ── Headers: cookie ──────────────────────────────────────────────────────
    let hCookie = Headers.cookie "session" :: Codec (Headers.Headers ForRequest) Int Int
    assertEq "cookie roundtrip"
        (Right 42)
        (fst (Headers.parse hCookie (Headers.print hCookie 42)))

    assertEq "cookie missing fails"
        (Left Headers.ParseError)
        (fst (Headers.parse hCookie []))

    -- ── Data: IsoPathData ────────────────────────────────────────────────────
    assertEq "IsoPathData Int"
        (Right (42 :: Int))
        (parseUrlPiece (toUrlPiece (42 :: Int)))

    assertEq "IsoPathData Text"
        (Right ("hello" :: Text))
        (parseUrlPiece (toUrlPiece ("hello" :: Text)))

    assertEq "IsoPathData Bool True"
        (Right True)
        (parseUrlPiece @Bool (toUrlPiece True))

    assertEq "IsoPathData Bool False"
        (Right False)
        (parseUrlPiece @Bool (toUrlPiece False))

    -- ── Data: IsoQueryData ───────────────────────────────────────────────────
    assertEq "IsoQueryData Int"
        (Right (42 :: Int))
        (parseQueryParam (toQueryParam (42 :: Int)))

    assertEq "IsoQueryData Text"
        (Right ("hello" :: Text))
        (parseQueryParam (toQueryParam ("hello" :: Text)))

    assertEq "IsoQueryData Maybe Int Nothing"
        (Right (Nothing :: Maybe Int))
        (parseQueryParam (toQueryParam (Nothing :: Maybe Int)))

    assertEq "IsoQueryData Maybe Int Just"
        (Right (Just 42 :: Maybe Int))
        (parseQueryParam (toQueryParam (Just 42 :: Maybe Int)))

    -- ── Data: IsoHeaderData ──────────────────────────────────────────────────
    assertEq "IsoHeaderData Int"
        (Right (42 :: Int))
        (parseHeader (toHeader (42 :: Int)))

    assertEq "IsoHeaderData Text"
        (Right ("hello" :: Text))
        (parseHeader (toHeader ("hello" :: Text)))

    -- ── Data: IsoCookieData ──────────────────────────────────────────────────
    assertEq "IsoCookieData Int"
        (Right (42 :: Int))
        (parseCookieValue (toCookieValue (42 :: Int)))

    assertEq "IsoCookieData Text"
        (Right ("hello" :: Text))
        (parseCookieValue (toCookieValue ("hello" :: Text)))

    assertEq "IsoCookieData Bool True"
        (Right True)
        (parseCookieValue (toCookieValue True))

    assertEq "IsoCookieData Bool False"
        (Right False)
        (parseCookieValue (toCookieValue False))
