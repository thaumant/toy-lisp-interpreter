module Repl (
    runREPL,
    evalAndPrint
) where


import System.IO (putStr, hFlush, stdout)
import Control.Monad.Except (throwError, runExceptT)
import Data
import Parse (readExpr)
import Eval (Env, eval, newEnv)


runREPL :: IO ()
runREPL = newEnv >>= runREPL'

evalAndPrint :: String -> IO ()
evalAndPrint input = newEnv >>= flip evalAndPrint' input

runREPL' :: Env -> IO ()
runREPL' env = do
    putStr "Lisp > "
    hFlush stdout
    input <- getLine
    if input == "exit" then return ()
    else evalAndPrint' env input >> runREPL' env

evalAndPrint' :: Env -> String -> IO ()
evalAndPrint' env input = printResult ((liftThrows . readExpr $ input) >>= eval env)
    where
        liftThrows :: Either LispError a -> CanThrow a
        liftThrows (Left err) = throwError err
        liftThrows (Right val) = return val

printResult :: CanThrow LispVal -> IO ()
printResult val = runExceptT val >>= (putStrLn . extractValue)
    where
        extractValue (Right val) = show val
        extractValue (Left err) = show err
