{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LinearTypes #-}
-- writeFile "pages/syntax.css" $ Pandoc.styleToCss syntaxStyle
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (someFunc) where

import Control.Monad.IO.Class qualified as IO
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.InterpolatedString.Perl6 qualified as Perl6
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Highlighting qualified as Pandoc
import Text.Pandoc.Lua qualified as Pandoc
import Text.Pandoc.Scripting qualified as Pandoc
import Text.Pretty.Simple qualified as Pretty

mdToHTML :: Text.Text -> IO Text.Text
mdToHTML txt =
  Pandoc.runIOorExplode $
    Pandoc.readCommonMark Pandoc.def txt
      >>= Pandoc.writeHtml5String Pandoc.def {Pandoc.writerHighlightStyle = Just syntaxStyle}

syntaxStyle = Pandoc.tango

pureAST :: Text.Text -> IO Pandoc.Pandoc
pureAST txt = Pandoc.runIOorExplode $ Pandoc.readCommonMark Pandoc.def txt

top =
  [Perl6.q|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Okapi Wiki</title>
    <link rel="stylesheet" href="style.css">
    <link rel="stylesheet" href="syntax.css">
  </head>
  <body>
  <div id="content">
|]

bot =
  [Perl6.q|
  </div>
  </body>
</html>
|]

someFunc :: IO ()
someFunc = do
  md <- Text.readFile "index.md"
  html <- mdToHTML md
  Text.writeFile "pages/index.html" (top <> html <> bot)
  ast <- pureAST md
  Pretty.pPrint ast
