module Data where


import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)


data LispVal = Symbol String
    | List [LispVal]
    | Integer Int
    | String String
    | Bool Bool

instance Show LispVal where
    show (Symbol s) = s
    show (List vs) = "(" ++ (unwords . map show $ vs) ++ ")"
    show (Integer i) = show i
    show (String s) = "\"" ++ s ++ "\""
    show (Bool b) = if b then "#t" else "#f"

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

instance Show LispError where
    show (NumArgs n found) = "Invalid number of arguments: expected " ++ show n ++ ", found " ++ (show $ List found)
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseError) = "Parse error at " ++ show parseError
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message fname) = message ++ ": " ++ show fname
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (Default message) = "An error has occured: " ++ message
