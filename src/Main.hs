module Main where


import           Evaluator
import           Parser
import           System.Environment
import           Text.ParserCombinators.Parsec (parse)


-- Main function for scheme parsing
main :: IO ()
main = getArgs >>= print . eval . readExpr . head


-- Reads and parses input expression
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val
