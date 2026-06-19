{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Okapi.Protocol.Request.Path qualified as Path
import Okapi.Protocol.Request.Query qualified as Query
import Okapi.Protocol.Shared.Headers qualified as Headers
-- Request (..) brings the field names into scope so OverloadedRecordDot's
-- magic HasField solver fires on req.path / req.query / req.headers, despite
-- Request using NoFieldSelectors. A plain `import Okapi` does this for users.
import Okapi
    ( LitF (..), ConstF (..), CookieF (..), SetCookieF (..)
    , Request (..)
    , mGet, pathOf, queryOf, headersOf, isoCodec
    )

-- ── Test records ──────────────────────────────────────────────────────────────

data TestPath = TestPath
    { _api    :: LitF "api"
    , version :: Text
    } deriving (Generic, Eq, Show)

data TestQuery = TestQuery
    { user_id :: Int
    , format  :: Maybe Text
    , verbose :: Maybe ()
    } deriving (Generic, Eq, Show)

-- bare () field exercises the required-flag generic instance
data FlagQuery = FlagQuery
    { active :: ()
    } deriving (Generic, Eq, Show)

data TestReqHeaders = TestReqHeaders
    { content_type :: ConstF "application/json"
    , x_auth_token :: Text
    , session      :: CookieF Text
    } deriving (Generic, Eq, Show)

-- two cookies in one record exercises cookie state re-threading
data TwoCookies = TwoCookies
    { sid   :: CookieF Text
    , theme :: CookieF Text
    } deriving (Generic, Eq, Show)

-- optional request cookie
data OptCookie = OptCookie
    { tok :: Maybe (CookieF Text)
    } deriving (Generic, Eq, Show)

-- response Set-Cookie (and its optional form)
data RespHeaders = RespHeaders
    { x_trace  :: Text
    , refresh  :: SetCookieF Text
    , maybe_sc :: Maybe (SetCookieF Text)
    } deriving (Generic, Eq, Show)


-- ── Helpers ───────────────────────────────────────────────────────────────────

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual
    | expected == actual = putStrLn $ "PASS: " <> label
    | otherwise          = error $ "FAIL: " <> label
                              <> "\n  expected: " <> show expected
                              <> "\n  actual:   " <> show actual

assertBool :: String -> Bool -> IO ()
assertBool label b
    | b         = putStrLn $ "PASS: " <> label
    | otherwise = error $ "FAIL: " <> label


-- ── Path: through the public `pathOf` combinator ──────────────────────────────

test_pathOf :: IO ()
test_pathOf = do
    let codec = isoCodec (mGet & pathOf @TestPath).path
    let val   = TestPath { _api = LitF, version = "v2" }

    assertEq "pathOf: print" ["api", "v2"] (Path.print codec val)
    assertEq "pathOf: round-trip" (Right val) (Path.parseExact codec (Path.print codec val))
    assertEq "pathOf: literal mismatch fails"
        (Left (Path.ParseError, ["wrong", "v2"]))
        (Path.parseExact codec ["wrong", "v2"])


-- ── Query: through the public `queryOf` combinator ────────────────────────────

test_queryOf :: IO ()
test_queryOf = do
    let codec = isoCodec (mGet & queryOf @TestQuery).query

    let val1 = TestQuery { user_id = 42, format = Just "json", verbose = Just () }
    assertEq "queryOf: all-present round-trip"
        (Right val1) (fst (Query.parse codec (Query.print codec val1)))

    let val2 = TestQuery { user_id = 7, format = Nothing, verbose = Nothing }
    assertEq "queryOf: optional-absent round-trip"
        (Right val2) (fst (Query.parse codec (Query.print codec val2)))

    -- required param missing must fail
    assertEq "queryOf: missing required param fails"
        (Left Query.ParseError) (fst (Query.parse codec [("format", Just "json")]))

    let printed1 = Query.print codec val1
    assertBool "queryOf: user_id key present" (any (\(k, _) -> k == "user_id") printed1)
    assertBool "queryOf: verbose is bare flag (no value)"
        (any (\(k, v) -> k == "verbose" && v == Nothing) printed1)

    -- bare () required-flag instance
    let fcodec = isoCodec (mGet & queryOf @FlagQuery).query
    assertEq "queryOf: bare () flag round-trip"
        (Right (FlagQuery ())) (fst (Query.parse fcodec (Query.print fcodec (FlagQuery ()))))
    assertEq "queryOf: bare () flag absent fails"
        (Left Query.ParseError) (fst (Query.parse fcodec []))


-- ── Headers: through the public `headersOf` combinator ────────────────────────

test_headersOf :: IO ()
test_headersOf = do
    let codec = isoCodec (mGet & headersOf @TestReqHeaders).headers

    let val = TestReqHeaders
                { content_type = ConstF
                , x_auth_token = "secret"
                , session      = CookieF "abc123"
                }
    let printed = Headers.print codec val

    assertBool "headersOf: field x_auth_token → header x-auth-token"
        (any (\(k, _) -> k == "x-auth-token") printed)
    assertBool "headersOf: content-type assertion printed"
        (any (\(k, v) -> k == "content-type" && v == "application/json") printed)

    assertEq "headersOf: round-trip" (Right val) (fst (Headers.parse codec printed))

    -- NEGATIVE: ConstF must reject a wrong value (the whole point of ConstF)
    let wrong = [ ("content-type", "text/html")
                , ("x-auth-token", "secret")
                , ("cookie", "session=abc123")
                ]
    assertEq "headersOf: ConstF rejects wrong value"
        (Left Headers.ParseError) (fst (Headers.parse codec wrong))

    -- NEGATIVE: ConstF must reject an absent assertion header
    let missing = [ ("x-auth-token", "secret"), ("cookie", "session=abc123") ]
    assertEq "headersOf: ConstF rejects absent header"
        (Left Headers.ParseError) (fst (Headers.parse codec missing))


-- ── Headers: multiple cookies in one record (state re-threading) ──────────────

test_twoCookies :: IO ()
test_twoCookies = do
    let codec = isoCodec (mGet & headersOf @TwoCookies).headers
    let val = TwoCookies { sid = CookieF "s1", theme = CookieF "dark" }
    assertEq "headersOf: two cookies round-trip"
        (Right val) (fst (Headers.parse codec (Headers.print codec val)))


-- ── Headers: optional request cookie ──────────────────────────────────────────

test_optCookie :: IO ()
test_optCookie = do
    let codec = isoCodec (mGet & headersOf @OptCookie).headers

    let present = OptCookie { tok = Just (CookieF "xyz") }
    assertEq "headersOf: optional cookie present round-trip"
        (Right present) (fst (Headers.parse codec (Headers.print codec present)))

    let absent = OptCookie { tok = Nothing }
    assertEq "headersOf: optional cookie absent round-trip"
        (Right absent) (fst (Headers.parse codec (Headers.print codec absent)))
    assertEq "headersOf: optional cookie absent from empty"
        (Right absent) (fst (Headers.parse codec []))


-- ── Headers: response Set-Cookie + optional Set-Cookie ────────────────────────
--   headersOf is request-only; the ForResponse instances are reached via
--   headersCodec @ForResponse directly.

test_respHeaders :: IO ()
test_respHeaders = do
    let codec = Headers.headersCodec @Headers.ForResponse @RespHeaders

    let val = RespHeaders
                { x_trace  = "t-1"
                , refresh  = SetCookieF "r1"
                , maybe_sc = Just (SetCookieF "m1")
                }
    let printed = Headers.print codec val
    assertBool "headersCodec(ForResponse): set-cookie printed"
        (any (\(k, _) -> k == "set-cookie") printed)
    assertEq "headersCodec(ForResponse): round-trip (Just)"
        (Right val) (fst (Headers.parse codec printed))

    let val2 = val { maybe_sc = Nothing }
    assertEq "headersCodec(ForResponse): round-trip (Nothing)"
        (Right val2) (fst (Headers.parse codec (Headers.print codec val2)))


-- ── Main ─────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    test_pathOf
    test_queryOf
    test_headersOf
    test_twoCookies
    test_optCookie
    test_respHeaders
    putStrLn "All generic tests passed."
