{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

--- !!!!!!TODO: HTTParser :TODO!!!!!!

module Okapi.QuasiQuotes where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.List.NonEmpty
import Data.Maybe (catMaybes, mapMaybe)
import Data.String (IsString)
import Data.Text
import GHC.ExecutionStack (Location (functionName))
import GHC.Unicode (isAscii)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Okapi (OkapiT, Response, MonadOkapi)
import System.Random

data RoutePart = Method Text | PathSegMatch Text | AnonPathSeg CurlyExpr | AnonQueryParam Text CurlyExpr | Bind Text
  deriving (Eq, Show)

newtype URL = URL {unURL :: Text}
  deriving newtype (IsString, Semigroup, Monoid, Eq, Ord, Show)

data Route m i o = Route
  { parser :: OkapiT m o
  , url    :: i -> URL
  }

parseCurlyExpr :: Parser CurlyExpr
parseCurlyExpr = between (char '{') (char '}') $ do
  typeName <- Data.Attoparsec.Text.takeWhile (\char -> isAlphaNum char || char == '[' || char == ']' || char == '(' || char == ')')
  transformFunctionNames <- many $ do
    string "->"
    Data.Attoparsec.Text.takeWhile isAlphaNum
  filterFunctionName <- optional $ do
    char '|'
    Data.Attoparsec.Text.takeWhile isAlphaNum
  pure $ CurlyExpr typeName transformFunctionNames filterFunctionName

routeParser :: Parser [RoutePart]
routeParser = many $ do
  skipSpace
  parseMethod <|> parsePathSegMatch <|> parseAnonPathSeg <|> parseAnonQueryParam <|> parseBind

parseMethod :: Parser RoutePart
parseMethod = do
  method <- Data.Attoparsec.Text.takeWhile isUpper
  case method of
    "GET" -> pure $ Method "GET"
    "HEAD" -> pure $ Method "HEAD"
    "POST" -> pure $ Method "POST"
    "DELETE" -> pure $ Method "DELETE"
    "PUT" -> pure $ Method "PUT"
    "PATCH" -> pure $ Method "PATCH"
    _ -> fail "Couldn't parse method"

parsePathSegMatch :: Parser RoutePart
parsePathSegMatch = do
  char '/'
  match <- Data.Attoparsec.Text.takeWhile1 isAlpha
  pure $ PathSegMatch match

parseAnonPathSeg :: Parser RoutePart
parseAnonPathSeg = do
  char '/'
  AnonPathSeg <$> parseCurlyExpr

parseAnonQueryParam :: Parser RoutePart
parseAnonQueryParam = do
  char '?'
  queryParamName <- Data.Attoparsec.Text.takeWhile isAlphaNum
  AnonQueryParam queryParamName <$> parseCurlyExpr

parseBind :: Parser RoutePart
parseBind = do
  string ">>="
  skipSpace
  functionName <- Data.Attoparsec.Text.takeWhile1 (\char -> isAlphaNum char || char == '.')
  pure $ Bind functionName

routePartsToExp :: [RoutePart] -> Q Exp
routePartsToExp [] =
  pure $
    RecConE
      (mkName "Route")
      [ (mkName "parser", VarE (mkName "Okapi.skip")),
        (mkName "url", LamE [VarP $ mkName "unit"] (AppE (ConE $ mkName "Okapi.URL") (LitE $ StringL "")))
      ]
routePartsToExp routeParts = do
  let binds = Prelude.dropWhile (not . isBind) routeParts
      notBinds = Prelude.takeWhile (not . isBind) routeParts
  routePartStmtsAndBindings <- mapM routePartStmtAndBinding notBinds
  let routePartStmts = Prelude.map (\(_, _, stmts) -> stmts) routePartStmtsAndBindings
      bindingsAndTypes = mapMaybe (\(bandTs, _, _) -> bandTs) routePartStmtsAndBindings
      bAndTHelper :: (Maybe (Name, Type), Maybe HTTPDataType, Stmt) -> Maybe (Maybe Name, HTTPDataType) = \case
        (Nothing, Just pathSegType@(PathSegType seg), _) -> Just (Nothing, pathSegType)
        (Just (name, _), Just httpDataType, _) -> Just (Just name, httpDataType)
        _ -> Nothing
      bindingsAndHTTPDataTypes :: [(Maybe Name, HTTPDataType)] = mapMaybe bAndTHelper routePartStmtsAndBindings
      bindings = Prelude.map fst bindingsAndTypes
      -- types = map snd bindingsAndTypes
      returnStmt :: Stmt =
        case bindings of
          [] -> NoBindS (AppE (VarE $ mkName "pure") (ConE $ mkName "()"))
          [binding] -> NoBindS (AppE (VarE $ mkName "pure") (VarE binding))
          _ -> NoBindS (AppE (VarE $ mkName "pure") (TupE (Prelude.map (Just . VarE) bindings)))
      leftSide = ParensE (DoE Nothing $ routePartStmts <> [returnStmt])
      (middle, rightSide) =
        case binds of
          [] -> (VarE $ mkName ">>=", LamE [VarP $ mkName "params"] (AppE (VarE $ mkName "pure") (VarE $ mkName "params")))
          [Bind functionName] -> (VarE $ mkName ">>=", VarE $ mkName $ unpack functionName)
          _ -> (VarE $ mkName ">>", AppE (VarE $ mkName "Okapi.throw") (VarE $ mkName "Okapi.internalServerError"))
  pure $
    RecConE
      (mkName "Okapi.Route")
      [ (mkName "parser", UInfixE leftSide middle rightSide),
        (mkName "url", LamE [lambdaPattern bindingsAndTypes] (lambdaBody True bindingsAndHTTPDataTypes))
      ]

isBind :: RoutePart -> Bool
isBind (Bind _) = True
isBind _ = False

-- bindsExp :: NonEmpty RoutePart -> Exp
-- bindsExp (Bind functionName) = VarE $ mkName $ unpack functionName
-- bindsExp ((Bind functionName) :| rps) = UInfixE (VarE $ mkName $ unpack functionName) (VarE $ mkName ">>=") (loop rps)
--   where
--     loop :: [RoutePart] -> Exp
--     loop [] = LamE [WildP] (VarE $ mkName "Okapi.skip")
--     loop ((Bind functionName) : rps) = undefined

lambdaPattern :: [(Name, Type)] -> Pat
lambdaPattern [] = WildP
lambdaPattern [(n, t)] = SigP (VarP n) t
lambdaPattern nAndTs = TupP $ Prelude.map (\(n, t) -> SigP (VarP n) t) nAndTs

data HTTPDataType = PathSegType Text | AnonPathParamType | AnonQueryParamType Text

isQueryParamType :: HTTPDataType -> Bool
isQueryParamType (AnonQueryParamType _) = True
isQueryParamType _ = False

lambdaBody :: Bool -> [(Maybe Name, HTTPDataType)] -> Exp
lambdaBody _ [] = AppE (ConE (mkName "Okapi.URL")) (LitE $ StringL "")
lambdaBody isFirstQueryParam (combo@(_, httpDataType) : combos) =
  UInfixE
    (helper isFirstQueryParam combo)
    (VarE $ mkName "<>")
    ( lambdaBody
        ( not (isQueryParamType httpDataType && isFirstQueryParam) && isFirstQueryParam
        )
        combos
    )
  where
    helper :: Bool -> (Maybe Name, HTTPDataType) -> Exp
    helper _ (Nothing, PathSegType match) = AppE (ConE (mkName "Okapi.URL")) (LitE $ StringL $ "/" <> unpack match)
    helper _ (Just name, AnonPathParamType) = AppE (ConE (mkName "Okapi.URL")) (UInfixE (LitE $ StringL "/") (VarE $ mkName "<>") (ParensE $ AppE (VarE $ mkName "toUrlPiece") (VarE name)))
    helper isFirstQueryParam' (Just name, AnonQueryParamType queryParamName) = AppE (ConE (mkName "Okapi.URL")) (UInfixE (LitE $ StringL $ unpack $ (if isFirstQueryParam' then "?" else "&") <> queryParamName <> "=") (VarE $ mkName "<>") (ParensE $ AppE (VarE $ mkName "toQueryParam") (VarE name)))
    helper _ _ = AppE (ConE (mkName "Okapi.URL")) (LitE $ StringL "")

routePartStmtAndBinding :: RoutePart -> Q (Maybe (Name, Type), Maybe HTTPDataType, Stmt)
routePartStmtAndBinding rp = case rp of
  Method m -> case m of
    "GET" -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.get"))
    "HEAD" -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.head"))
    "POST" -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.post"))
    "DELETE" -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.delete"))
    "PUT" -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.put"))
    "PATCH" -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.patch"))
    _ -> pure (Nothing, Nothing, NoBindS (VarE $ mkName "Okapi.skip"))
  PathSegMatch txt -> pure (Nothing, Just $ PathSegType txt, NoBindS (AppE (VarE $ mkName "Okapi.pathSeg") (LitE $ StringL $ unpack txt)))
  AnonPathSeg (CurlyExpr typeName functionNamesToApply maybeGuardFunction) -> do
    stmtBinding <- runIO randName
    let stmt = BindS (SigP (VarP stmtBinding) (ConT $ mkName $ unpack typeName)) (VarE (mkName "Okapi.pathParam"))
    pure (Just (stmtBinding, ConT $ mkName $ unpack typeName), Just AnonPathParamType, stmt)
  AnonQueryParam queryParamName (CurlyExpr typeName functionNamesToApply maybeGuardFunction) -> do
    stmtBinding <- runIO randName
    let stmt = BindS (SigP (VarP stmtBinding) (ConT $ mkName $ unpack typeName)) (AppE (VarE (mkName "Okapi.queryParam")) (LitE $ StringL $ unpack queryParamName))
    pure (Just (stmtBinding, ConT $ mkName $ unpack typeName), Just $ AnonQueryParamType queryParamName, stmt)
  Bind functionName -> pure (Nothing, Nothing, NoBindS $ AppE (VarE $ mkName "Okapi.throw") (VarE $ mkName "Okapi.internalServerError"))

randName :: IO Name
randName = do
  str <- fmap (Prelude.take 10 . randomRs ('a', 'z')) newStdGen
  pure $ mkName str

data CurlyExpr
  = CurlyExpr
      Text -- type name
      [Text] -- transform function names
      (Maybe Text) -- filter function name
  deriving (Eq, Show)

genRoute :: QuasiQuoter
genRoute =
  QuasiQuoter
    { quoteExp = genRouteExp . pack,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }
  where
    genRouteExp :: Text -> Q Exp
    genRouteExp txt = do
      let parserResult = parseOnly routeParser txt
      case parserResult of
        Left _ -> routePartsToExp []
        Right routeParts -> routePartsToExp routeParts

test1 :: IO ()
test1 = do
  let result = parseOnly routeParser "GET HEAD /movies /{Date|isModern} ?director{Director} ?actors{[Actor]->childActors->bornInIndiana|notEmpty} ?female{Gender}"
      goal =
        Right
          [ Method "GET",
            Method "HEAD",
            PathSegMatch "movies",
            AnonPathSeg (CurlyExpr "Date" [] (Just "isModern")),
            AnonQueryParam "director" (CurlyExpr "Director" [] Nothing),
            AnonQueryParam "actors" (CurlyExpr "[Actor]" ["childActors", "bornInIndiana"] (Just "notEmpty")),
            AnonQueryParam "female" (CurlyExpr "Gender" [] Nothing)
          ]
  if result == goal
    then print "PASSED!"
    else print "FAILED!"

{-
newtype URL = URL Text

data URLParam = forall a. ToHttpApiData a => URLParam a

-- Maybe should just generate parser? But I thought the point was to get info for URL?
-- Maybe instead of generating URL have URL builder helpers?
data Route input = Route
  { parser :: Okapi Response
  , url    :: input -> URL
  }

sequencing routes
route1 <> route2 = Route
  { parser = do
      route1
      route2
  , url =
  }

data RouteParams = forall a. RouteParams (Map Text a)

-- (%%) :: (a -> URL) -> (b -> URL) -> (a -> b -> URL)
-- (%%) urlProducer urlProducer' = \a -> \b -> urlProducer a <> urlProducer' b

-- Example Usage

newtype BookID = BookID Int deriving (FromHttpApiData)

newtype Author = Author Text deriving (FromHttpApiData)

data URL = Empty | PathPiece Text | QueryPiece Text Text | Path URL URL

TODO: What TO DO with partial URLs???????????

cons :: URL -> URL -> URL
cons Empty Empty            = Empty
cons Empty pp@(PathPiece _) = Path Empty pp
cons Empty p@(Path _ _)     = p
cons pp1@(Path _ _)

-- Expand on type safe URL building. Will need it either way

data Genre = Fantasy | SciFi | NonFiction | Romance
-}

{-
-- booksRoute :: ? -- Type isn't known until code is generated at compile time
booksRoute = [Okapi.route|GET /books /{Maybe BookID} ?author{Maybe Author} ?genre{Maybe Genre}|]

booksURL :: URL
booksURL = booksRoute.url (Nothing, Just "Mark Twain", Nothing)

booksRouteNamed = [Okapi.genRoute|GET /books /:bookID ?author:authorName ?genre:|]

newProduct :: Okapi Response
newProduct = do
  validProductID <- [Okapi.genParser|POST /products/{ProductID|isValid}|]
  ...

books :: Okapi Response
books = booksRoute.parser >>= booksHandler
  where
    booksHandler :: (Maybe BookID, Maybe Author, Maybe Genre) -> Okapi Response
    booksHandler (maybeBookID, maybeAuthor, maybeGenre) = do
      ...

isModern :: Date -> Bool

childActors :: [Actor] -> [Actor]

bornInIndiana :: [Actor] -> [Actor]

-- Could be gen parser
-- parser generator for headers?
moviesRoute = [Okapi.route|
    GET
    HEAD
    /movies
    /{Date|isModern}
    ?director{Director}
    ?actors{[Actor]->childActors->bornInIndiana|notEmpty}
    ?female
|]

"GET HEAD /movies /{Date|isModern} ?director{Director} ?actors{[Actor]->childActors->bornInIndiana|notEmpty} ?female{Gender}"

THE ABOVE MAY BE PARTIAL ROUTES WITHOUT EVERY PIECE OF A URL

---------------------
-- Multiple Routes --
---------------------

[Okapi.genRoute|
  CHOOSE
    GET /                                        >> getHome
    GET /movies             >> authenticate      >> getMoviesHandler
    PUT /movies /{MovieID} >>= authenticateData >>= putMovieHandler
    GET /books              >> authenticate      >> getBooksHandler
    PUT /books /{BookID}   >>= authenticateData >>= putBookHandle
    GET /anotherPath >> anotherHandler

    someOtherRoute = ...
|]

authenticate :: Okapi ()

authenticateData :: forall a. a -> Okapi a

getMoviesHandler :: Okapi Response
getBooksHandler  :: Okapi Response
putMovieHandler  :: MovieID -> Okapi Response
putBookHandler   :: BookID  -> Okapi Response
anotherHandler   :: Okapi Response

-- Generates:
megaApp :: Okapi Response
megaApp = app <|> anotherApp

app :: Okapi Response -- top level parser
app = getMovieRoute.parser <|> putMovieRoute.parser <|> ...

anotherApp :: Okapi Response
anotherApp = anotherRoute.parser

getMoviesRoute
putMovieRoute
getBooksRoute
putBookRoute
anotherRoute

TODO: generate Haddock documentation too!
https://gitlab.haskell.org/ghc/ghc/-/commit/8a59f49ae2204dbf58ef50ea8c0a50ee2c7aa64a
-}
{-
-- genRoute creates a full route. Must have at least a method and return a response handler.
genRoute :: Q Exp
genRoute = undefined

-- Can use any part of the routing syntax to create a parser, but no URL is created just a parser.
genParser :: Q Exp
genParser = undefined

-- Generate entire top level parser. Basically multiple routes
genApp :: Q [Dec]
genApp = undefined

genOptimalApp :: Q [Dec]
genOptimalApp = undefined
-}