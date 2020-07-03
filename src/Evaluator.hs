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
