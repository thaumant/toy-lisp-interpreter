module Repl (
    runREPL,
    evalAndPrint
) where


import System.IO (putStr, hFlush, stdout)
import Data
import Parse (readExpr)
import Eval (eval)


readPrompt :: String -> IO String
readPrompt prompt = do
    putStr prompt
    hFlush stdout
    getLine

printResult :: Either LispError LispVal -> IO ()
printResult (Left err) = putStrLn $ show err
printResult (Right val) = putStrLn $ show val

evalAndPrint :: String -> IO ()
evalAndPrint input = printResult (readExpr input >>= eval)

runREPL :: IO ()
runREPL = do
    input <- readPrompt "Lisp > "
    if input == "exit" then return ()
    else evalAndPrint input >> runREPL
