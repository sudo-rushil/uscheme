module Evaluator where


import           Parser


-- Evaluator Primitives

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval val@(Character _)          = val
eval val@(Float _)              = val
eval val@(Ratio _)              = val
eval val@(Complex _)            = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args -- function application


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("symbol?", unaryOp symbolp)
    , ("string?", unaryOp stringp)
    , ("number?", unaryOp numberp)
    , ("bool?", unaryOp boolp)
    , ("list?", unaryOp listp)
    , ("symbol->string", unaryOp symbol2string)
    , ("string->symbol", unaryOp string2symbol)
    ]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v -- error to be given more than 2 args


symbolp, stringp, numberp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
boolp (Bool _) = Bool True
boolp _        = Bool False
listp (List _) = Bool True
listp _        = Bool False


symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _        = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""


-- Instances

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ (unwords . map show) contents ++ ")"
    show (DottedList head tail) = "(" ++ (unwords . map show) head ++ " . " ++ show tail ++ ")"


-- data LispVal = Atom String √
--     | List [LispVal]
--     | DottedList [LispVal] LispVal
--     | Number Integer √
--     | String String √
--     | Bool Bool √
--     | Character Char
--     | Float Double
--     | Ratio Rational
--     | Complex (Complex Double)
--     | Vector (Array Int LispVal) √
