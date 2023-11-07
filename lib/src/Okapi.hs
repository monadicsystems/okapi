{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Okapi where

import Control.Natural qualified as Natural
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSChar8
import Data.Functor.Identity qualified as Identity
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Typeable qualified as Typeable
import Data.Vault.Lazy qualified as Vault
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Okapi.App
import Okapi.App qualified as App
import Okapi.Headers qualified as Headers
import Okapi.Route qualified as Route
import Okapi.Secret qualified as Secret
import Text.Pretty.Simple qualified as Pretty
import Web.HttpApiData qualified as Web

{-
test1 :: IO ()
test1 = do
  apiTreeRep <- forest testAPI
  putStrLn $ Tree.drawTree apiTreeRep
  where
    -- Warp.run 1234 $ (build testAPI id) backupWaiApp

    backupWaiApp = \req resp -> do
      resp $ Wai.responseLBS HTTP.status200 [] "The test app failed..."
    testAPI :: [App]
    testAPI =
      [ lit
          "" -- Won't be matched because you can't request http://localhost:1234/
          [ get_ id \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "The trailing slash"
          ],
        lit
          "hello"
          [ get_ id \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "world",
            lit
              ""
              [ get_ id \req -> do
                  return $ Wai.responseLBS HTTP.status200 [] "Trailing slash after \"hello\""
              ],
            lit
              "world"
              [ get_ id \req -> do
                  return $ Wai.responseLBS HTTP.status200 [] "!"
              ]
          ],
        get_ id \req -> do
          return $ Wai.responseLBS HTTP.status200 [] "You made a GET request to :ROOT:"
      ]

test2 :: IO ()
test2 = do
  apiTreeRep <- forest testAPI
  putStrLn $ Tree.drawTree apiTreeRep
  where
    -- Warp.run 1234 $ (build testAPI id) backupWaiApp

    backupWaiApp = \req resp -> do
      resp $ Wai.responseLBS HTTP.status200 [] "The test app failed..."
    testAPI :: [App]
    testAPI =
      lit
        "" -- Won't be matched because you can't request http://localhost:1234/
        [ get_ id \req -> do
            return $ Wai.responseLBS HTTP.status200 [] "The trailing slash"
        ]
        : lit
          "hello"
          [ get_ id \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "world",
            lit
              ""
              [ get_ id \req -> do
                  return $ Wai.responseLBS HTTP.status200 [] "Trailing slash after \"hello\""
              ],
            lit
              "world"
              [ get_ id \req -> do
                  return $ Wai.responseLBS HTTP.status200 [] "!"
              ]
          ]
        : ( get_ id \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "You made a GET request to :ROOT:"
          )
        : []

test3 :: IO ()
test3 = do
  apiTreeRep <- forest testAPI
  putStrLn $ Tree.drawTree apiTreeRep
  where
    -- Warp.run 1234 $ (build testAPI id) backupWaiApp

    backupWaiApp = \_ resp -> do
      resp $ Wai.responseLBS HTTP.status200 [] "The test app failed..."
    testAPI :: [App]
    testAPI =
      [ lit
          "numbers"
          [ lit
              "add"
              [ param @Int \xS ->
                  [ param @Int \yS ->
                      [ getIO_ \req -> do
                          let magic = Secret.tell req
                              x = magic xS
                              y = magic yS
                          return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show (x + y)
                      ]
                  ]
              ],
            getIO_ \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "Use /add, /sub, or /mul"
          ]
      ]

data Op = Add | Sub | Mul

instance Web.FromHttpApiData Op where
  parseUrlPiece "add" = Right Add
  parseUrlPiece "sub" = Right Sub
  parseUrlPiece "mul" = Right Mul
  parseUrlPiece _ = Left undefined

test4 :: IO ()
test4 = do
  apiTreeRep <- forest testAPI
  putStrLn $ Tree.drawTree apiTreeRep
  where
    -- Warp.run 1234 $ Wai.logStdoutDev $ build testAPI id backupWaiApp

    backupWaiApp = \_ resp -> do
      resp $ Wai.responseLBS HTTP.status404 [] "The test app failed..."
    testAPI :: [App]
    testAPI =
      [ lit
          "numbers"
          [ param @Op \opS ->
              [ param @Int \xS ->
                  [ param @Int \yS ->
                      [ getIO_ \req -> do
                          let x = Secret.tell req xS
                              y = Secret.tell req yS
                              answer = case Secret.tell req opS of
                                Add -> x + y
                                Sub -> x - y
                                Mul -> x * y
                          return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show answer
                      ]
                  ],
                getIO_ \req -> do
                  return $ Wai.responseLBS HTTP.status200 [] $ case Secret.tell req opS of
                    Add -> "Add two numbers."
                    Sub -> "Subtract one number from another."
                    Mul -> "Multiply two numbers."
              ],
            getIO_ \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "Use /add, /sub, or /mul"
          ]
      ]

instance Web.ToHttpApiData Op where
  toUrlPiece Add = "add"
  toUrlPiece Sub = "sub"
  toUrlPiece Mul = "mul"

test5 :: IO ()
test5 = do
  apiTreeRep <- forest testAPI
  -- apiEndpoints <- endpoints testAPI
  putStrLn $ Tree.drawTree apiTreeRep
  where
    -- Pretty.pPrint $ map curl $ List.reverse apiEndpoints

    -- Warp.run 1234 $ build testAPI id backupWaiApp

    backupWaiApp = \_ resp -> do
      resp $ Wai.responseLBS HTTP.status404 [] "The test app failed..."
    testAPI :: [App]
    testAPI =
      [ lit "numbers" $
          [ getIO_ \req -> do
              return $ Wai.responseLBS HTTP.status200 [] "Use /add, /sub, or /mul"
          ]
            ++ map opAPI [Add, Sub, Mul]
      ]

opAPI :: Op -> App
opAPI op =
  match
    op
    [ getIO_ \req -> do
        return $ Wai.responseLBS HTTP.status200 [] $ case op of
          Add -> "Add two numbers."
          Sub -> "Subtract one number from another."
          Mul -> "Multiply two numbers.",
      param @Int \xS ->
        [ param @Int \yS ->
            [ getIO_ \req -> do
                let x = Secret.tell req xS
                    y = Secret.tell req yS
                    answer = case op of
                      Add -> x + y
                      Sub -> x - y
                      Mul -> x * y
                return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show answer
            ]
        ]
          ++ case op of
            Mul ->
              [ getIO_ \req -> do
                  let x = Secret.tell req xS
                  return $ Wai.responseLBS HTTP.status200 [] $ LBSChar8.pack $ show (x * x)
              ]
            _ -> []
    ]
-}
-- test6 :: IO ()
-- test6 = do
--   apiTreeRep <- forest testAPI
--   putStrLn $ Tree.drawTree apiTreeRep
--   where
--     backupWaiApp = \req resp -> do
--       resp $ Wai.responseLBS HTTP.status200 [] "The test app failed..."
--     testAPI :: [App]
--     testAPI =
--       [ endpoint HTTP.GET (do Route.lit "user";) id \_ req -> do
--           undefined
--       , endpoint HTTP.POST (do Route.lit "user"; id' <- Route.param @Int; return id') id \userIDS req -> do
--           let userID = Secret.tell req userIDS
--           undefined
--       ]
