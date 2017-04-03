module Main where


import Parse
import System.Environment
import Text.ParserCombinators.Parsec (parse)


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn . show . parse parseExpr "lisp" $ expr
