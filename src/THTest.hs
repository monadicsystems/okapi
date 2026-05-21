{-# LANGUAGE TemplateHaskell #-}

module THTest where

import Language.Haskell.TH.Syntax
import TH
import Control.Monad.IO.Class

liftThenShow :: App -> Q ()
liftThenShow app = do
    appAST <- lift app
    liftIO $ print appAST
    return ()

