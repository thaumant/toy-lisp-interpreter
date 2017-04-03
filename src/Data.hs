module Data where

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Integer Int
    | Float Float
    | String String
    | Bool Bool
    deriving Show