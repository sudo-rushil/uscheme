module Parser where


import           Text.ParserCombinators.Parsec hiding (spaces)


-- Data Types


-- Parsers

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<+>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space
