{-# LANGUAGE ScopedTypeVariables #-}

module Okapi.HSP where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.Attoparsec.Text as Atto
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Traversable as Traversable
import qualified Language.Haskell.Meta as Meta
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Okapi
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath


data DirTree = Dir Text.Text [DirTree] | File Text.Text Text.Text
  deriving (Show, Eq)

buildDirTree' root = do
  executableDir <- IO.liftIO Directory.getCurrentDirectory
  dirTree <- buildDirTree root
  IO.liftIO $ Directory.setCurrentDirectory executableDir
  pure dirTree

buildDirTree :: IO.MonadIO m => FilePath -> m DirTree
buildDirTree root = do
  rootContents <- IO.liftIO $ Directory.listDirectory root
  dirTrees <- Monad.forM rootContents $ \content -> do
    case FilePath.takeExtension content of
      ""  -> do
        IO.liftIO $ Directory.setCurrentDirectory root
        buildDirTree content
      ext -> pure $ File (Text.pack $ FilePath.dropExtension content) (Text.pack ext)
  pure $ Dir (Text.pack root) dirTrees

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

getAST :: IO.MonadIO m => FilePath -> m (Exts.ParseResult (Exts.Exp Exts.SrcSpanInfo))
getAST hspDirectory = do
  currentDirectory <- IO.liftIO $ Directory.getCurrentDirectory
  let hspDirAbsPath = currentDirectory FilePath.</> hspDirectory
  fullPaths <- IO.liftIO $ getFullPaths hspDirAbsPath
  fullPathsWithParseResult <- IO.liftIO $ Monad.forM fullPaths $ \fullPath -> do
    fileContents <- readFile $ hspDirAbsPath FilePath.</> (FilePath.joinPath fullPath)
    let indentedFileContents = unlines $ map (\line -> "  " <> line) $ lines fileContents
        doBlock = "do\n" <> indentedFileContents
    pure (fullPath, Exts.parseExpWithMode (Exts.defaultParseMode {Exts.extensions = [Exts.EnableExtension Exts.TypeApplications, Exts.EnableExtension Exts.BlockArguments]}) doBlock)
  let (_, someExp) = head fullPathsWithParseResult
  pure someExp

parseExp :: String -> Either String TH.Exp
parseExp = either Left (Right . Meta.toExp) . Meta.parseResultToEither . Exts.parseExpWithMode (Exts.defaultParseMode {Exts.extensions = [Exts.EnableExtension Exts.TypeApplications, Exts.EnableExtension Exts.BlockArguments]})

generateParser' :: FilePath -> TH.Q String
generateParser' hspDirectory = do
  currentDirectory <- IO.liftIO $ Directory.getCurrentDirectory
  let hspDirAbsPath = currentDirectory FilePath.</> hspDirectory
  fullPaths <- IO.liftIO $ getFullPaths hspDirAbsPath
  fullPathsWithExp :: [([FilePath], TH.Exp)] <- IO.liftIO $ Monad.forM fullPaths $ \fullPath -> do
    fileContents <- readFile $ hspDirAbsPath FilePath.</> (FilePath.joinPath fullPath)
    let indentedFileContents = unlines $ map (\line -> "  " <> line) $ lines fileContents
        doBlock = "do\n" <> indentedFileContents
    putStrLn doBlock
    case parseExp doBlock of
      Left _ -> error "The do block isn't formatted correctly!"
      Right exp -> pure (fullPath, exp)
  let (_, someExp) = head fullPathsWithExp
      pretty = TH.pprint someExp
  pure pretty

-- TODO: DO REMAINDER OF WORK HERE
generateParser :: FilePath -> TH.Q TH.Exp
generateParser hspDirectory = do
  currentDirectory <- IO.liftIO $ Directory.getCurrentDirectory
  let hspDirAbsPath = currentDirectory FilePath.</> hspDirectory
  fullPaths <- IO.liftIO $ getFullPaths hspDirAbsPath
  fullPathsWithExp :: [([FilePath], TH.Exp)] <- IO.liftIO $ Monad.forM fullPaths $ \fullPath -> do
    fileContents <- readFile $ hspDirAbsPath FilePath.</> (FilePath.joinPath fullPath)
    let
      indentedFileContents = unlines $ map (\line -> "  " <> line) $ lines fileContents
      doBlock = "do/n" <> indentedFileContents
    case parseExp doBlock of
      Left _ -> error "The do block isn't formatted correctly!"
      Right exp@(TH.DoE stmts) -> do
        let
          prefixStmts :: [TH.Stmt] = fullPath <&> \path ->
            if isPathParam $ FilePath.dropExtension path
            then undefined
            else undefined
        pure (fullPath, exp)
      _ -> error "This isn't a do block!"
  undefined

isPathParam :: FilePath -> Bool
isPathParam (h:t) =
  if h == '['
  then
    if (dropWhile (']' /=) t) == "]"
    then True
    else False
  else False
isPathParam _ = False

isDirectory :: FilePath -> Bool
isDirectory filePath =
  if (dropWhile ('/' /=) filePath) == "/"
    then True
    else False
