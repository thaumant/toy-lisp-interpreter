module Data where


import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)


data LispVal = Atom String
    | List [LispVal]
    | Integer Int
    | Float Float
    | String String
    | Bool Bool

instance Show LispVal where
    show (Atom s) = s
    show (List vs) = "(" ++ (unwords . map show $ vs) ++ ")"
    show (Integer i) = show i
    show (Float f) = show f
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
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseError) = "Parse error at " ++ show parseError
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message fname) = message ++ ": " ++ show fname
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (Default message) = "An error has occured: " ++ message
