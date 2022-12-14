module Thompson2 (regexParser) where
import           Control.Monad                      (msum)
import           Data.Char                          (ord)
import qualified Text.ParserCombinators.Parsec      as P
import qualified Text.ParserCombinators.Parsec.Expr as PE

data Node = Concat !Node !Node | Star !Node | Or !Node !Node | Character !Int deriving(Show)

regexParser :: P.Parser Node
regexParser = PE.buildExpressionParser opTable base
  where
    opTable = [   [ PE.Postfix (P.char '*' >> return Star)]
                , [ PE.Infix (return Concat) PE.AssocLeft]
                , [ PE.Infix (P.char '|' >> return Or) PE.AssocLeft]
              ]
    base = msum [Character . ord <$> P.noneOf "()*|", parens regexParser]
    parens = P.between (P.char '(') (P.char ')')
