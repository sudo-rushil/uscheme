module Main where


import           Control.Monad.Except
import           Evaluator
import           Parser
import           System.Environment
import           Text.ParserCombinators.Parsec (parse)


-- Main function for scheme parsing
main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled


-- Reads and parses input expression
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
