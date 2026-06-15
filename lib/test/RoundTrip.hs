{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Internal qualified as WaiI
import Okapi
import Okapi.Headers qualified as ReqH
import Okapi.Request.Method qualified as Method
import Okapi.Request.Path qualified as Path
import Okapi.Request.Query qualified as Query
import Okapi.Response.Status qualified as Status
import System.Exit (exitFailure)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
    | expected == actual = putStrLn ("PASS: " ++ name)
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual:   " ++ show actual)
        exitFailure

assertRight :: Show e => String -> Either e a -> (a -> IO ()) -> IO ()
assertRight name (Left e) _ = do
    putStrLn ("FAIL: " ++ name ++ " - unexpected Left: " ++ show e)
    exitFailure
assertRight name (Right x) k = do
    putStrLn ("PASS: " ++ name)
    k x

waiResBody :: Wai.Response -> LBS.ByteString
waiResBody (WaiI.ResponseBuilder _ _ b) = Builder.toLazyByteString b
waiResBody _                            = LBS.empty

test_methodRoundTrip :: IO ()
test_methodRoundTrip = do
    let codec = Method.method Method.GET
    let printed = Method.print codec Method.GET
    assertEq "method: print GET" "GET" printed
    let (parsed, _) = Method.parse codec printed
    assertEq "method: parse GET" (Right Method.GET) parsed

    let codec2 = Method.method Method.DELETE
    let printed2 = Method.print codec2 Method.DELETE
    assertEq "method: print DELETE" "DELETE" printed2
    let (parsed2, _) = Method.parse codec2 printed2
    assertEq "method: parse DELETE" (Right Method.DELETE) parsed2

test_pathRoundTrip :: IO ()
test_pathRoundTrip = do
    let req = mGet & path do
            _ <- seg_ @Text "users"
            userId <- seg @Text "userId"
            pure userId
    let pathCodec = isoCodec req.path
    let printed = Path.print pathCodec "alice"
    assertEq "path: print [users,alice]" ["users", "alice"] printed
    let (parsed, _) = Path.parse pathCodec printed
    assertEq "path: parse alice" (Right "alice") parsed

test_queryRoundTrip :: IO ()
test_queryRoundTrip = do
    let pCodec = Query.param @Text "filter"
    let printed = Query.print pCodec "hello"
    assertEq "query: param print" [("filter", Just "hello")] printed
    let (parsed, _) = Query.parse pCodec printed
    assertEq "query: param parse" (Right "hello") parsed

    let oCodec = Query.param' @Text "search"
    let printedN = Query.print oCodec Nothing
    assertEq "query: param' Nothing print" [] printedN
    let (parsedN, _) = Query.parse oCodec printedN
    assertEq "query: param' Nothing parse" (Right Nothing) parsedN
    let printedJ = Query.print oCodec (Just "foo")
    assertEq "query: param' Just print" [("search", Just "foo")] printedJ
    let (parsedJ, _) = Query.parse oCodec printedJ
    assertEq "query: param' Just parse" (Right (Just "foo")) parsedJ

    let fCodec = Query.flag "active"
    let printedF = Query.print fCodec ()
    assertEq "query: flag print" [("active", Nothing)] printedF
    let (parsedF, _) = Query.parse fCodec printedF
    assertEq "query: flag parse" (Right ()) parsedF

    let foCodec = Query.flag' "debug"
    let printedFT = Query.print foCodec True
    assertEq "query: flag' True print" [("debug", Nothing)] printedFT
    let (parsedFT, _) = Query.parse foCodec printedFT
    assertEq "query: flag' True parse" (Right True) parsedFT
    let printedFF = Query.print foCodec False
    assertEq "query: flag' False print" [] printedFF
    let (parsedFF, _) = Query.parse foCodec printedFF
    assertEq "query: flag' False parse" (Right False) parsedFF

test_headersRoundTrip :: IO ()
test_headersRoundTrip = do
    let hCodec = ReqH.header @Text "x-api-key"
    let printed = ReqH.print hCodec "secret"
    assertEq "headers: header print" [("x-api-key", "secret")] printed
    let (parsed, _) = ReqH.parse hCodec printed
    assertEq "headers: header parse" (Right "secret") parsed

    let oCodec = ReqH.header' @Text "x-opt"
    let printedN = ReqH.print oCodec Nothing
    assertEq "headers: header' Nothing print" [] printedN
    let (parsedN, _) = ReqH.parse oCodec printedN
    assertEq "headers: header' Nothing parse" (Right Nothing) parsedN
    let printedJ = ReqH.print oCodec (Just "val")
    assertEq "headers: header' Just print" [("x-opt", "val")] printedJ
    let (parsedJ, _) = ReqH.parse oCodec printedJ
    assertEq "headers: header' Just parse" (Right (Just "val")) parsedJ

    let cCodec = ReqH.cookie @Text "session"
    let printedC = ReqH.print cCodec "abc123"
    assertEq "headers: cookie print" [("cookie", "session=abc123")] printedC
    let (parsedC, _) = ReqH.parse cCodec printedC
    assertEq "headers: cookie parse" (Right "abc123") parsedC

test_statusRoundTrip :: IO ()
test_statusRoundTrip = do
    let c200 = Status.status S200
    let p200 = Status.print c200 S200
    assertEq "status: print S200" HTTP.status200 p200
    let (r200, _) = Status.parse c200 p200
    assertEq "status: parse S200" (Right S200) r200

    let c404 = Status.status S404
    let p404 = Status.print c404 S404
    assertEq "status: print S404" HTTP.status404 p404
    let (r404, _) = Status.parse c404 p404
    assertEq "status: parse S404" (Right S404) r404

    let c500 = Status.status S500
    let p500 = Status.print c500 S500
    assertEq "status: print S500" HTTP.status500 p500
    let (r500, _) = Status.parse c500 p500
    assertEq "status: parse S500" (Right S500) r500

data GetUserRes f
    = OkRes       (Response f S200 (Text, Text) LBS.ByteString)
    | NotFoundRes (Response f S404 Int LBS.ByteString)
    | ErrorRes    (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, ResponseEnum)

endpoint =
    ( mGet
      & path do
          _ <- seg_ @Text "users"
          uid <- seg @Text "uid"
          pure uid
      & query (param' @Text "filter")
    ) :->
    responsesOf @GetUserRes
        (s200 & headers do
            ct  <- fst =. header @Text "content-type"
            loc <- snd =. header @Text "location"
            pure (ct, loc))
        (s404 & headers (header @Int "retry-after"))
        s500

test_reqRoundTrip :: IO ()
test_reqRoundTrip = do
    let rv = request GET "alice" (Just "active") [] (pure "request-body")
    waiReq <- printRequest endpoint rv
    reqBody <- Wai.strictRequestBody waiReq
    assertEq "req: method" "GET"              (Wai.requestMethod waiReq)
    assertEq "req: path"   ["users", "alice"] (Wai.pathInfo      waiReq)
    assertEq "req: query"  [("filter", Just "active")] (Wai.queryString waiReq)
    assertEq "req: body"   "request-body"     reqBody
    case parseRequest endpoint waiReq of
        Left _     -> do { putStrLn "FAIL: req parse"; exitFailure }
        Right parsed -> do
            putStrLn "PASS: req parse"
            assertEq "req: path value" ("alice" :: Text) (value parsed.path)

test_resRoundTrip :: IO ()
test_resRoundTrip = do
    let okVal = OkRes (response S200 ("text/html", "/home") (pure "response-body"))
    waiRes <- printResponse endpoint okVal
    assertEq "res: ok status"  HTTP.status200 (Wai.responseStatus  waiRes)
    assertEq "res: ok body"    "response-body" (waiResBody waiRes)
    case parseResponse endpoint waiRes of
        Nothing -> do { putStrLn "FAIL: res: ok parse"; exitFailure }
        Just _  -> putStrLn "PASS: res: ok reconstruct"

    let nfVal = NotFoundRes (response S404 (42 :: Int) (pure "not-found-body"))
    waiRes2 <- printResponse endpoint nfVal
    assertEq "res: 404 status"  HTTP.status404   (Wai.responseStatus waiRes2)
    assertEq "res: 404 body"    "not-found-body" (waiResBody waiRes2)
    case parseResponse endpoint waiRes2 of
        Nothing -> do { putStrLn "FAIL: res: 404 parse"; exitFailure }
        Just _  -> putStrLn "PASS: res: 404 reconstruct"

test_serverClientRoundTrip :: IO ()
test_serverClientRoundTrip = do
    mgr <- HC.newManager HC.defaultManagerSettings
    Warp.testWithApplication (pure testApp) $ \port -> do
        let reqVal = request GET "alice" (Just "active") [] (pure mempty)
        result <- fetch mgr ("http://localhost:" ++ show port) endpoint reqVal
        case result of
            Left e -> do
                putStrLn ("FAIL: server/client roundtrip: " ++ show e)
                exitFailure
            Right (OkRes resVal) -> do
                assertEq "server/client: status" S200 resVal.status.value
                assertEq "server/client: headers" ("text/html", "/home" :: Text) resVal.headers.value
                resBody <- resVal.body.value
                assertEq "server/client: body" "hello" resBody
            Right _ -> do
                putStrLn "FAIL: server/client: unexpected response variant"
                exitFailure
  where
    testApp = serve id endpoint (fn $ \_ ->
        pure (OkRes (response S200 ("text/html", "/home") (pure "hello"))))

main :: IO ()
main = do
    test_methodRoundTrip
    test_pathRoundTrip
    test_queryRoundTrip
    test_headersRoundTrip
    test_statusRoundTrip
    test_reqRoundTrip
    test_resRoundTrip
    test_serverClientRoundTrip
    putStrLn "all round-trip tests passed"
