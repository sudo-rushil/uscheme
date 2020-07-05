{-# LANGUAGE FlexibleInstances #-}

module Parser where


import           Control.Monad.Except
import           Data.Array
import           Data.Complex
import           Data.IORef
import           Data.Ratio
import           Numeric
import           System.IO
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
    | Vector (Array Int LispVal)
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func
    { params  :: [String]
    , vararg  :: (Maybe String)
    , body    :: [LispVal]
    , closure :: Env
    }
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle
    deriving (Eq)


type Env = IORef [(String, IORef LispVal)]


-- Error Types

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String


type ThrowsError = Either LispError


type IOThrowsError = ExceptT LispError IO


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
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> parseUnQuoteSplicing
        <|> try (do
                    string "#("
                    x <- parseVector
                    char ')'
                    return x)
        <|> parseList


parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ Atom atom


parseList :: Parser LispVal
parseList = between beg end parseList'
    where
        beg = (char '(' >> skipMany space)
        end = (skipMany space >> char ')')


parseList' :: Parser LispVal
parseList' = do
        list <- sepEndBy parseExpr spaces
        maybeDatum <- optionMaybe (char '.' >> spaces >> parseExpr)
        return $ case maybeDatum of
            Nothing    -> List list
            Just datum -> DottedList list datum


parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]


parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
        char '`'
        x <- parseExpr
        return $ List [Atom "quasiquote", x]


parseUnQuote :: Parser LispVal
parseUnQuote = do
        char ','
        x <- parseExpr
        return $ List [Atom "unquote", x]


parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
        string ",@"
        x <- parseExpr
        return $ List [Atom "unquote-splicing", x]


parseDottedList :: Parser LispVal
parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail


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


parseVector :: Parser LispVal
parseVector = do
        arrayVals <- sepBy parseExpr spaces
        return $ Vector (listArray (0, length arrayVals - 1) arrayVals)


-- Parser helpers

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val


-- Instances

instance Eq ([LispVal] -> ThrowsError LispVal) where
    _ == _ = True -- never do this


instance Eq ([LispVal] -> IOThrowsError LispVal) where
    _ == _ = True
