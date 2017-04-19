module Eval (
    eval,
    newEnv,
    Env
) where

import Data
import Control.Monad (mapM, liftM)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

newEnv :: IO Env
newEnv = mapM fromPrimitive primitives >>= newIORef
    where
        fromPrimitive (name, fn) = do
            ref <- newIORef (PrimitiveFunc fn)
            return (name, ref)

getVar :: Env -> String -> CanThrow LispVal
getVar envRef name = do
    env <- liftIO . readIORef $ envRef
    case lookup name env of
        (Just ref) -> liftIO . readIORef $ ref
        Nothing -> throwError $ UnboundVar "Referencing an unbound variable" name

setVar :: Env -> String -> LispVal -> CanThrow LispVal
setVar envRef name val = do
    env <- liftIO . readIORef $ envRef
    case lookup name env of
        (Just ref) -> liftIO $ writeIORef ref val
        Nothing -> throwError $ UnboundVar "Setting an unbound variable" name
    return val

defineVar :: Env -> String -> LispVal -> CanThrow LispVal
defineVar envRef name val = do
    env <- liftIO . readIORef $ envRef
    case lookup name env of
        (Just ref) -> liftIO $ writeIORef ref val
        Nothing -> do
            ref <- liftIO $ newIORef val
            liftIO $ writeIORef envRef ((name, ref) : env)
    return val

eval :: Env -> LispVal -> CanThrow LispVal
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Bool _) = return val
eval env (Symbol name) = getVar env name
eval env (List [Symbol "quote", val]) = return val
eval env (List [Symbol "if", cond, left, right]) = do
    result <- eval env cond
    case result of
        Bool True -> eval env left
        Bool False -> eval env right
        invalid -> throwError $ TypeMismatch "bool" invalid
eval env (List (Symbol "cond" : clauses)) = evalCond env clauses
eval env (List [Symbol "set!", Symbol name, val]) = eval env val >>= setVar env name
eval env (List (Symbol "lambda" : List args : body)) = return $ Func (map show args) body env
eval env (List [Symbol "define", Symbol name, val]) = eval env val >>= defineVar env name
eval env (List (Symbol fname : args)) = do
    fn <- getVar env fname
    args <- mapM (eval env) args
    applyFunc fn args
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyFunc :: LispVal -> [LispVal] -> CanThrow LispVal
applyFunc (PrimitiveFunc fn) params = fn params
applyFunc (Func args body closure) params = do
        if length args /= length params then throwError $ NumArgs (toInteger $ length args) params
        else do
            env <- liftIO . bindVars closure $ zip args params
            liftM last . mapM (eval env) $ body
    where
        bindVars :: Env -> [(String, LispVal)] -> IO Env
        bindVars envRef params = readIORef envRef >>= extendEnv params >>= newIORef
            where
                extendEnv params env = liftM (++ env) . mapM addBinding $ params
                addBinding (name, val) = do
                    ref <- newIORef val
                    return (name, ref)

evalCond :: Env -> [LispVal] -> CanThrow LispVal
evalCond env (List (Symbol "else" : branch : []) : _) = eval env branch
evalCond env (List (cond : branch : []) : rest) = do
    result <- eval env cond
    case result of
        Bool True -> eval env branch
        Bool False -> evalCond env rest
        invalid -> throwError $ TypeMismatch "bool" invalid
evalCond env (pair : rest) = throwError $ TypeMismatch "pair ('condition 'branch)" pair
evalCond env [] = throwError $ Default "Exhausted cond clauses"

primitives :: [(String, [LispVal] -> CanThrow LispVal)]
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

numFn :: (Int -> Int -> Int) -> [LispVal] -> CanThrow LispVal
numFn fn args = mapM unpackNum args >>= return . Integer . foldl1 fn

numBoolBinOp  = boolBinOp unpackNum
strBoolBinOp  = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

boolBinOp :: (LispVal -> CanThrow a) -> (a -> a -> Bool) -> [LispVal] -> CanThrow LispVal
boolBinOp unpack op args =
    if length args /= 2 then throwError $ NumArgs 2 args
    else do
        left  <- unpack $ args !! 0
        right <- unpack $ args !! 1
        return . Bool $ left `op` right

unpackNum :: LispVal -> CanThrow Int
unpackNum (Integer n) = return n
unpackNum notNum = throwError $ TypeMismatch "integer" notNum

unpackStr :: LispVal -> CanThrow String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> CanThrow Bool
unpackBool (Bool s) = return s
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

car :: [LispVal] -> CanThrow LispVal
car [List (x:_)] = return x
car [art] = throwError $ TypeMismatch "pair" art
car args  = throwError $ NumArgs 1 args

cdr :: [LispVal] -> CanThrow LispVal
cdr [List (_:xs)] = return $ List xs
cdr [art] = throwError $ TypeMismatch "pair" art
cdr args  = throwError $ NumArgs 1 args

cons :: [LispVal] -> CanThrow LispVal
cons [x, List xs] = return $ List $ x : xs
cons [x, y] = throwError $ TypeMismatch "list" y
cons args = throwError $ NumArgs 2 args

eqv :: [LispVal] -> CanThrow LispVal
eqv [Integer x,   Integer y]   = return . Bool $ x == y
eqv [String x,    String y]    = return . Bool $ x == y
eqv [Bool x,      Bool y]      = return . Bool $ x == y
eqv [Symbol x,    Symbol y]    = return . Bool $ x == y
eqv [List [],     List []]     = return . Bool $ True
eqv [List (x:xs), List (y:ys)] = do
    eqvFirst <- eqv [x, y]
    case eqvFirst of
        (Bool True) -> eqv [List xs, List ys]
        _ -> return . Bool $ False
eqv [_, _] = return . Bool $ False
eqv args = throwError $ NumArgs 2 args