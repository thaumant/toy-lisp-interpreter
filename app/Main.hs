module Main where


import System.Environment
import Text.ParserCombinators.Parsec (parse)
import Parse
import Eval


main :: IO ()
main = getArgs >>= putStrLn . show . fmap eval . parse parseExpr "lisp" . head
