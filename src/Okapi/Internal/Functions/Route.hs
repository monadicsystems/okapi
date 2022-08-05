{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Okapi.Internal.Functions.Route where

import Control.Monad.Combinators
import qualified Data.Attoparsec.Text as Atto
import Data.Char
import Data.Maybe
import Data.Text
import Language.Haskell.TH
import Okapi.Internal.Types
import System.Random

parseCurlyExpr :: Atto.Parser CurlyExpr
parseCurlyExpr = between (Atto.char '{') (Atto.char '}') $ do
  typeName <- Atto.takeWhile (\char -> isAlphaNum char || char == '[' || char == ']' || char == '(' || char == ')')
  transformFunctionNames <- many $ do
    Atto.string "->"
    Atto.takeWhile isAlphaNum
  filterFunctionName <- optional $ do
    Atto.char '|'
    Atto.takeWhile isAlphaNum
  pure $ CurlyExpr typeName transformFunctionNames filterFunctionName

routeParser :: Atto.Parser [RoutePart]
routeParser = many $ do
  Atto.skipSpace
  parseMethod <|> parsePathSegMatch <|> parseAnonPathSeg <|> parseAnonQueryParam <|> parseBind

parseMethod :: Atto.Parser RoutePart
parseMethod = do
  method <- Atto.takeWhile isUpper
  case method of
    "GET" -> pure $ Method "GET"
    "HEAD" -> pure $ Method "HEAD"
    "POST" -> pure $ Method "POST"
    "DELETE" -> pure $ Method "DELETE"
    "PUT" -> pure $ Method "PUT"
    "PATCH" -> pure $ Method "PATCH"
    _ -> fail "Couldn't parse method"

parsePathSegMatch :: Atto.Parser RoutePart
parsePathSegMatch = do
  Atto.char '/'
  match <- Atto.takeWhile1 isAlpha
  pure $ PathSegMatch match

parseAnonPathSeg :: Atto.Parser RoutePart
parseAnonPathSeg = do
  Atto.char '/'
  AnonPathSeg <$> parseCurlyExpr

parseAnonQueryParam :: Atto.Parser RoutePart
parseAnonQueryParam = do
  Atto.char '?'
  queryParamName <- Atto.takeWhile isAlphaNum
  AnonQueryParam queryParamName <$> parseCurlyExpr

parseBind :: Atto.Parser RoutePart
parseBind = do
  Atto.string ">>="
  Atto.skipSpace
  functionName <- Atto.takeWhile1 (\char -> isAlphaNum char || char == '.')
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
