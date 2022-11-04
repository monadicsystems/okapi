module Okapi.Style.Sinatra.Quotes where

{-|

A quasiquoter for generating a path and query parser

Syntax of the DSL that describes the parser:

<empty>          - represents []
/                - represents [""]
/foo             - represents a path segment
/foo/bar         - represents ["foo", "bar"]
/foo/bar/:Int    - returns an Int
/foo/:Text/:Int  - returns (Text, Int)
/foo/:?Text/:Int - returns (Maybe Text, Int) (uses optional for parsing 2nd path segment)
/foo/:?Text/:?Int?name=:Text&hello=:?Bool - returns (Maybe Text, Maybe Int, Text, Maybe Bool)
/foo/:Int/*      - returns (Int, [Text])
/foo/*/:Bool/baz - returns ([Text], Bool)

The tuple is determined by the order of the types in the path description
-}

route :: QuasiQuoter
route =
  QuasiQuoter
    { quoteExp = genRouteExp . pack,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }
  where
    genRouteExp :: Text -> Q Exp
    genRouteExp txt = do
      let parserResult = Atto.parseOnly routeParser txt
      case parserResult of
        Left _ -> routePartsToExp []
        Right routeParts -> routePartsToExp routeParts
