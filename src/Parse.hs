module Parse (
    parseExpr,
    parseList,
    parseQuoted,
    parseBool,
    parseSymbol,
    parseString,
    parseInteger,
    readExpr
) where


import Text.ParserCombinators.Parsec
import Control.Arrow (left)
import Data


readExpr :: String -> Either LispError LispVal
readExpr input = left Parser (parse parseExpr "lisp" input)

parseExpr :: Parser LispVal
parseExpr = parseInteger
    <|> parseString
    <|> parseBool
    <|> parseSymbol
    <|> parseQuoted
    <|> parseList

parseList :: Parser LispVal
parseList = do
    char '('
    exprs <- parseExpr `sepBy` (skipMany1 space)
    char ')'
    return $ List exprs

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    expr <- parseExpr
    return $ List [Symbol "quote", expr]

parseString :: Parser LispVal
parseString = do
    char '"'
    str <- many ((char '\\' >> anyChar >>= return . escapedChar) <|> noneOf "\\\"")
    char '"'
    return . String $ str

escapedChar :: Char -> Char
escapedChar 'n' = '\n'
escapedChar 'r' = '\r'
escapedChar 't' = '\t'
escapedChar '\\' = '\\'
escapedChar char = char

parseBool :: Parser LispVal
parseBool = do
    val <- (try $ string "#t") <|> string "#f"
    return . Bool $ case val of
        "#t" -> True
        _ -> False

parseSymbol :: Parser LispVal
parseSymbol = do
    first <- letter <|> symbol
    rest <- many $ symbol <|> digit <|> letter
    return . Symbol $ first:rest

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseInteger :: Parser LispVal
parseInteger = many1 digit >>= return . Integer . read

-- parseFloat :: Parser LispVal
-- parseFloat = try $ do
--     intPart <- many1 digit
--     char '.'
--     floatPart <- many1 digit
--     return . Float . read $ (intPart ++ "." ++ floatPart)
