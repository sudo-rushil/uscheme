module Parser where


import           Data.Complex
import           Data.Ratio
import           Numeric
import           Text.ParserCombinators.Parsec hiding (spaces)

-- Data Types

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)


-- Parsers

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter


parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseDecimal' <|> parseHex <|> parseOct <|> parseBin


parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)


parseDecimal' :: Parser LispVal
parseDecimal' = try (string "#d") >> parseDecimal


parseHex :: Parser LispVal
parseHex = try (string "#x") >> many1 hexDigit >>= (return . Number . hex2dig)


parseOct :: Parser LispVal
parseOct = try (string "#o") >> many1 octDigit >>= (return . Number . oct2dig)


parseBin :: Parser LispVal
parseBin = try $ string "#b" >> many1 (oneOf "10") >>= (return . Number . bin2dig)


-- conversion functions
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs


parseString :: Parser LispVal
parseString = do
        char '"'
        x <- many $ escapedChars <|> noneOf "\"\\"
        char '"'
        return $ String x


escapedChars :: Parser Char
escapedChars = do
        char '\\'
        x <- oneOf "\\\"nrt"
        return $ case x of
            '\\' -> x
            '"'  -> x
            'n'  -> '\n'
            'r'  -> '\r'
            't'  -> '\t'


parseBool :: Parser LispVal
parseBool = do
        char '#'
        (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


parseCharacter :: Parser LispVal
parseCharacter = do
        try $ string "#\\"
        value <- try (string "newline" <|> string "space")
                <|> do { x <- anyChar; notFollowedBy alphaNum; return [x]}
        return $ Character $ case value of
            "space"   -> ' '
            "newline" -> '\n'
            _         -> (value !! 0)


parseFloat :: Parser LispVal
parseFloat = do
        x <- many1 digit
        char '.'
        y <- many1 digit
        return $ Float (fst.head $ readFloat (x++"."++y))


parseRatio :: Parser LispVal
parseRatio = do
        x <- many1 digit
        char '/'
        y <- many1 digit
        return $ Ratio ((read x) % (read y))


parseComplex :: Parser LispVal
parseComplex = do
        x <- (try parseFloat <|> parseDecimal)
        char '+'
        y <- (try parseFloat <|> parseDecimal)
        char 'i'
        return $ Complex (toDouble x :+ toDouble y)


toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n


-- Parser helpers

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space
