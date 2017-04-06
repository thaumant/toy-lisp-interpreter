module Eval (
    eval
) where

import Data
import Control.Monad (mapM)
import Control.Monad.Except (throwError)

eval :: LispVal -> Either LispError LispVal
eval val@(String _) = return val
eval val@(Integer _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom fname : args)) = mapM eval args >>= applyFunc fname -- applyFunc fname $ map eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyFunc :: String -> [LispVal] -> Either LispError LispVal
applyFunc fname args = maybe
                        (throwError $ NotFunction "Unrecognized primitive function args" fname)
                        ($ args)
                        (lookup fname primitives)

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives = [
        ("+", numFn (+)),
        ("-", numFn (-)),
        ("*", numFn (*))
        -- ("/", numFn div),
        -- ("mod", numFn mod),
        -- ("quotient", numFn quot),
        -- ("remainder", numFn rem)
    ]

numFn :: (Int -> Int -> Int) -> [LispVal] -> Either LispError LispVal
numFn fn args = mapM unpackNum args >>= return . Integer . foldl1 fn

unpackNum :: LispVal -> Either LispError Int
unpackNum (Integer n) = Right n
unpackNum notNum = throwError $ TypeMismatch "integer" notNum
