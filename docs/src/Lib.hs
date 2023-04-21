{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
-- writeFile "pages/syntax.css" $ Pandoc.styleToCss syntaxStyle
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (someFunc) where

import Control.Monad.IO.Class qualified as IO
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.InterpolatedString.Perl6 qualified as Perl6
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Highlighting qualified as Pandoc
import Text.Pandoc.Scripting qualified as Pandoc
import Text.Pandoc.Walk qualified as Pandoc
import Text.Pretty.Simple qualified as Pretty

mdToHTML :: Pandoc.Pandoc -> IO Text.Text
mdToHTML ast =
  Pandoc.runIOorExplode do
    htmlTxt <- Pandoc.writeHtml5String (Pandoc.def {Pandoc.writerHighlightStyle = Just syntaxStyle}) ast
    pure htmlTxt

syntaxStyle = Pandoc.tango

pureAST :: Text.Text -> IO Pandoc.Pandoc
pureAST txt = Pandoc.runIOorExplode $ Pandoc.readCommonMark Pandoc.def txt

makeTop :: [(Text.Text, Text.Text)] -> Text.Text
makeTop links =
  [Perl6.qc|
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
  <nav>
      <ul>
        {Text.intercalate "\\n" $ map linkHtml links}
      </ul>
  </nav>
  <main>
|]
  where
    linkHtml :: (Text.Text, Text.Text) -> Text.Text
    linkHtml (i, t) = [Perl6.qc|<li><a href="#{i}">{t}</a></li>|]

bot =
  [Perl6.q|
  </main>
  </body>
</html>
|]

(|>) = (&)

someFunc :: IO ()
someFunc = do
  md <- Text.readFile "index.md"
  ast <- pureAST md
  let modifiedAst =
        ast
          |> Pandoc.walk addHeaderID
      findLinks (Pandoc.Header lvl (i, _, _) ils) =
        if lvl == 2
          then [(i, Pandoc.query blockText ils |> Text.intercalate " ")]
          else []
      findLinks _ = []
      links = Pandoc.query findLinks modifiedAst

      top = makeTop links
  html <- mdToHTML modifiedAst
  Text.writeFile "pages/index.html" (top <> html <> bot)

addHeaderID :: Pandoc.Block -> Pandoc.Block
addHeaderID block@(Pandoc.Header lvl (_, c, kv) ils) =
  if lvl == 2
    then Pandoc.Header lvl (i', c, kv) ils
    else block
  where
    i' =
      Pandoc.query blockText ils
        |> map Text.toLower
        |> Text.intercalate ""
addHeaderID block = block

blockText = \case
  Pandoc.Str str -> [str]
  _ -> []

-- Text.writeFile "Report.hs" $ Text.pack $ show ast