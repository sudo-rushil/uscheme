module Main where


import           Parser
import           System.Environment
import           Text.ParserCombinators.Parsec (parse)


-- Main function for scheme parsing
main :: IO ()
main = do
        (expr:_) <- getArgs
        putStrLn (readExpr expr)


-- Reads and parses input expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"
