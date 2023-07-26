module Lexer (
    lexTokens
)
where

import Tokens
import Proc

isLetter :: Char -> Bool
isLetter c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

lexNumber :: String -> Proc (Double, String)
lexNumber tokens = do
    (lexedDigits, remainder) <- lexDigits tokens False
    let number = read lexedDigits :: Double
    return (number, remainder)

lexDigits :: String -> Bool -> Proc (String, String)
-- string is the tokens
-- is weather or not we have seen a decimal point

lexDigits [] _ = Suc ("", [])
lexDigits (x:xs) seenDecimal
  | x == '.' = if seenDecimal
                then Err "More than one decimal point in a number is not allowed"
                else do
                    (lexedDigits, remainder) <- lexDigits xs True
                    return (x:lexedDigits, remainder)
  | isDigit x = do
            (lexedDigits, remainder) <- lexDigits xs seenDecimal
            return (x:lexedDigits, remainder)
  | otherwise = return ("", x:xs)


lexString :: String -> Proc (String, String)
-- first string is the lexed string, second string is the remainder of the input
lexString [] = Err "Missing closing quotation"
lexString (x : xs)
  | x == '"' = return ("", x:xs)
  | not (isLetter x) = Err "Invalid character in string"
  | otherwise = do
        (lexedString, remainder) <- lexString xs
        return (x:lexedString, remainder)

lexTokens :: String -> Proc [Token]
lexTokens [] = Suc []
lexTokens s =
    case s of
        ' ' : xs -> lexTokens xs
        'f' : 'a' : 'l' : 's' : 'e' : xs -> do
            remainderTokens <- lexTokens xs
            return (FALSE:remainderTokens)
        't' : 'r' : 'u' : 'e' : xs -> do
            remainderTokens <- lexTokens xs
            return (TRUE:remainderTokens)
        'n' : 'u' : 'l' : 'l' : xs -> do
            remainderTokens <- lexTokens xs
            return (NULL:remainderTokens)
        x : xs ->
            if isLetter x
                then do
                    (lexedString, remainder) <- lexString (x:xs)
                    let newToken = STRING lexedString in do
                        remainderTokens <- lexTokens remainder
                        return (newToken:remainderTokens)
            else if isDigit x
                then do
                    (number, remainder) <- lexNumber (x:xs)
                    let newToken = NUMBER number in do
                        remainderTokens <- lexTokens remainder
                        return (newToken:remainderTokens)
                else
                    let newToken = case x of
                            '{' -> LBRACE
                            '}' -> RBRACE
                            '[' -> LBRACKET
                            ']' -> RBRACKET
                            '"' -> QUOTATION
                            ',' -> COMMA
                            ':' -> COLON
                            _ -> error "Invalid token"
                    in do
                        remainderTokens <- lexTokens xs
                        return (newToken:remainderTokens)