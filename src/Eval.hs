module Eval (
    eval
) where

import Data
import Control.Monad (mapM)
import Control.Monad.Except (throwError)

eval :: LispVal -> Either LispError LispVal
eval val@(String _) = return val
eval val@(Integer _) = return val
eval val@(Bool _) = return val
eval (List [Symbol "quote", val]) = return val
eval (List [Symbol "if", cond, left, right]) = do
    result <- eval cond
    case result of
        Bool True -> eval left
        Bool False -> eval right
        invalid -> throwError $ TypeMismatch "bool" invalid
eval (List (Symbol "cond" : clauses)) = evalCond clauses
eval (List (Symbol fname : args)) = mapM eval args >>= applyFunc fname
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyFunc :: String -> [LispVal] -> Either LispError LispVal
applyFunc fname args = maybe
                        (throwError $ NotFunction "Unrecognized primitive function args" fname)
                        ($ args)
                        (lookup fname primitives)

evalCond :: [LispVal] -> Either LispError LispVal
evalCond (List (Symbol "else" : branch : []) : _) = eval branch
evalCond (List (cond : branch : []) : rest) = do
    result <- eval cond
    case result of
        Bool True -> eval branch
        Bool False -> evalCond rest
        invalid -> throwError $ TypeMismatch "bool" invalid
evalCond (pair : rest) = throwError $ TypeMismatch "pair ('condition 'branch)" pair
evalCond [] = throwError $ Default "Exhausted cond clauses"

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives = [
        ("+", numFn (+)),
        ("-", numFn (-)),
        ("*", numFn (*)),
        ("<", numBoolBinOp (<)),
        (">", numBoolBinOp (>)),
        ("/=", numBoolBinOp (/=)),
        (">=", numBoolBinOp (>=)),
        ("<=", numBoolBinOp (<=)),
        ("&&", boolBoolBinOp (&&)),
        ("||", boolBoolBinOp (||)),
        ("string=?", strBoolBinOp (==)),
        ("string<?", strBoolBinOp (<)),
        ("string>?", strBoolBinOp (>)),
        ("string<=?", strBoolBinOp (<=)),
        ("string>=?", strBoolBinOp (>=)),
        ("car", car),
        ("cdr", cdr),
        ("cons", cons),
        ("=", eqv)
    ]

numFn :: (Int -> Int -> Int) -> [LispVal] -> Either LispError LispVal
numFn fn args = mapM unpackNum args >>= return . Integer . foldl1 fn

numBoolBinOp  = boolBinOp unpackNum
strBoolBinOp  = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

boolBinOp :: (LispVal -> Either LispError a) -> (a -> a -> Bool) -> [LispVal] -> Either LispError LispVal
boolBinOp unpack op args =
    if length args /= 2 then throwError $ NumArgs 2 args
    else do
        left  <- unpack $ args !! 0
        right <- unpack $ args !! 1
        return . Bool $ left `op` right

unpackNum :: LispVal -> Either LispError Int
unpackNum (Integer n) = return n
unpackNum notNum = throwError $ TypeMismatch "integer" notNum

unpackStr :: LispVal -> Either LispError String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> Either LispError Bool
unpackBool (Bool s) = return s
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

car :: [LispVal] -> Either LispError LispVal
car [List (x:_)] = return x
car [art] = throwError $ TypeMismatch "pair" art
car args  = throwError $ NumArgs 1 args

cdr :: [LispVal] -> Either LispError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [art] = throwError $ TypeMismatch "pair" art
cdr args  = throwError $ NumArgs 1 args

cons :: [LispVal] -> Either LispError LispVal
cons [x, List xs] = return $ List $ x : xs
cons [x, y] = throwError $ TypeMismatch "list" y
cons args = throwError $ NumArgs 2 args

eqv :: [LispVal] -> Either LispError LispVal
eqv [Integer x,   Integer y]   = Right . Bool $ x == y
eqv [String x,    String y]    = Right . Bool $ x == y
eqv [Bool x,      Bool y]      = Right . Bool $ x == y
eqv [Symbol x,    Symbol y]    = Right . Bool $ x == y
eqv [List [],     List []]     = Right . Bool $ True
eqv [List (x:xs), List (y:ys)] = case eqv [x, y] of
    Left err -> Right . Bool $ False
    Right (Bool False) -> Right . Bool $ False
    Right (Bool True) -> eqv [List xs, List ys]
eqv [_, _] = Right . Bool $ False
eqv args = throwError $ NumArgs 2 args