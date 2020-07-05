module Main where



import           Evaluator
import           Parser
import           System.Environment


-- Main

main :: IO ()
main = do
        args <- getArgs
        if null args then runRepl else runOne $ args
