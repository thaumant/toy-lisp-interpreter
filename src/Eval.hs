module Eval (
    eval
) where

import Data

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom fname : args)) = applyFunc fname $ map eval args
eval _ = undefined

applyFunc :: String -> [LispVal] -> LispVal
applyFunc fname args = maybe (Bool False) ($ args) $ lookup fname primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
        ("+", numFn (+)),
        ("-", numFn (-)),
        ("*", numFn (*))
        -- ("/", numFn div),
        -- ("mod", numFn mod),
        -- ("quotient", numFn quot),
        -- ("remainder", numFn rem)
    ]

numFn :: (Int -> Int -> Int) -> [LispVal] -> LispVal
numFn fn args = Integer . foldl1 fn . map unpackNum $ args

unpackNum :: LispVal -> Int
unpackNum (Integer n) = n
unpackNum _ = undefined
