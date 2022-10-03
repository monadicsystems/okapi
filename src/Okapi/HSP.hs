{-# LANGUAGE ScopedTypeVariables #-}

module Okapi.HSP where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.Attoparsec.Text as Atto
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Okapi
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath

{-| This quasiquoter generates a parser using HSP files.
It parses a root directory and does everything from there.
If no root directory is passed in, the default is "/hsp".
Must use trailing slash!
-}
hsp :: TH.QuasiQuoter
hsp =
  TH.QuasiQuoter
    { TH.quoteExp  = generateParser,
      TH.quotePat  = undefined,
      TH.quoteType = undefined,
      TH.quoteDec  = undefined
    }

generateParser :: FilePath -> TH.Q TH.Exp
generateParser hspDirectory = do
  currentDirectory <- IO.liftIO $ Directory.getCurrentDirectory
  fullPaths <- IO.liftIO $ getFullPaths $ currentDirectory FilePath.</> hspDirectory
  undefined

getFullPaths :: IO.MonadIO m => FilePath -> m [[FilePath]]
getFullPaths root = do
  fullPathsWithRoot <- loop root
  let
    rootSplit :: [FilePath] = FilePath.splitPath root
    splitFullPathsWithRoot :: [[FilePath]] = map FilePath.splitPath fullPathsWithRoot
    splitFullPathsWithoutRoot :: [[FilePath]] = map (\splitFullPathWithRoot -> splitFullPathWithRoot List.\\ rootSplit) splitFullPathsWithRoot
  pure splitFullPathsWithoutRoot
  where
    loop :: IO.MonadIO m => FilePath -> m [FilePath]
    loop root' = do
      rootContents <- IO.liftIO $ Directory.listDirectory root'
      tree <- Monad.forM rootContents $ \content -> do
        case FilePath.takeExtension content of
          "" -> loop $ root' FilePath.</> content
          _ -> pure [root' FilePath.</> content]
      pure $ concat tree
