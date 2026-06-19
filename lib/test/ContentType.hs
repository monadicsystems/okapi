{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid (p_)
import Okapi
import Okapi.Protocol.Shared.Body qualified as Body
import Okapi.Protocol.Shared.Headers qualified as H
import System.Exit (exitFailure)
import Web.FormUrlEncoded (FromForm, ToForm)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

data J = J { jx :: Int } deriving (Generic)
instance ToJSON J
instance FromJSON J
instance ToSchema J

data P = P { px :: Text } deriving (Generic)
instance ToForm P
instance FromForm P

main :: IO ()
main = do
    -- ── JSON request body folds in Content-Type: application/json ──────────────
    let jreq = mPost & body (json @J)
    let jh = isoCodec jreq.headers
    assertEq "json req emits content-type"
        [("content-type", "application/json")]
        (H.print jh [])
    -- lenient on receive: tolerate the charset parameter
    assertEq "json req parse tolerates ; charset"
        (Right [])
        (fst (H.parse jh [("content-type", "application/json; charset=utf-8")]))
    -- the assertion really runs: absent / wrong media type fail
    assertEq "json req parse rejects absent content-type"
        (Left H.ParseError)
        (fst (H.parse jh []))
    assertEq "json req parse rejects wrong content-type"
        (Left H.ParseError)
        (fst (H.parse jh [("content-type", "text/plain")]))

    -- ── Form request body → application/x-www-form-urlencoded ──────────────────
    let freq = mPost & body (form @P)
    let fh = isoCodec freq.headers
    assertEq "form req emits content-type"
        [("content-type", "application/x-www-form-urlencoded")]
        (H.print fh [])

    -- ── HTML response body → text/html ─────────────────────────────────────────
    let hres = s200 & body html
    let rh = isoCodec hres.headers
    assertEq "html res emits content-type"
        [("content-type", "text/html")]
        (H.print rh [])
    -- the HTML body actually renders lucid markup to bytes
    htmlBytes <- Body.printM Body.html (pure (p_ "hi"))
    assertEq "html body renders markup" "<p>hi</p>" (LBS.toStrict htmlBytes)

    putStrLn "all content-type tests passed"
