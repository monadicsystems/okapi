module Simple.Query where

import qualified Result

data Parser a

data ParserError = ParserError

match :: Parser a -> Result.Result ParserError a
match parser = undefined