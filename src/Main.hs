module Main where


import           Control.Monad.Except
import           Evaluator
import           Parser
import           System.Environment
import           Text.ParserCombinators.Parsec (parse)


-- Main function for scheme parsing
main :: IO ()
main = getArgs >>= print . eval . readExpr . head


-- Reads and parses input expression
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwsError $ Parser show err
    Right val -> return val
