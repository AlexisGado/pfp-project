module Thompson (showParser) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List ( intercalate )
import GHC.RTS.Flags (ProfFlags(doHeapProfile))
import Data.Char (ord)

data Node = Concat Node Node | Star Node | Or Node Node | Character Int

showParser :: Parser Node 
showParser =
	parenParser <|> -- ( ... )
	kleeneParser <|> -- a*
    charParser <|>    -- single char
	concatParser <|> -- ab
	orParser <?> "Parse error"

orParser :: Parser Node
orParser = do
	left <- showParser
	symbol "|"
	right <- showParser
	return (Or left right)

kleeneParser :: Parser Node
kleeneParser = do
	left <- showParser
	symbol "*"
	return (Star left)

concatParser :: Parser Node
concatParser = do
	left <- showParser
	right <- charParser
	return (Concat left right)

charParser :: Parser Node
charParser = do
	left <- character
	return (Character (ord left))

parenParser :: Parser Node
parenParser = do
	ls <- parens showParser
	return ls


lexer = P.makeTokenParser emptyDef
parens = P.parens lexer
character = P.charLiteral lexer
symbol = P.symbol lexer
