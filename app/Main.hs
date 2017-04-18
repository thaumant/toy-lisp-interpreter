{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import System.Environment
import Repl (runREPL, evalAndPrint)


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runREPL
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn $ "Expected 0 or 1 argument, " ++ (show . length $ args) ++ " given"
