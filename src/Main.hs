module Main where



import           Evaluator
import           Parser
import           System.Environment


-- Main

main :: IO ()
main = do
        args <- getArgs
        case length args of
            0 -> runRepl
            1 -> evalAndPrint $ args !! 0
            _ -> putStrLn "uScheme only takes 0 or 1 argument"
