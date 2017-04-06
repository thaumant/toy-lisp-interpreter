module Main where


import System.Environment
import Text.ParserCombinators.Parsec (parse)
import Parse (readExpr)
import Eval


main :: IO ()
main = do
    (input:_) <- getArgs
    let result = readExpr input >>= eval
    putStrLn $ show result
-- main = getArgs >>= putStrLn . show . fmap eval . parse parseExpr "lisp" . head
