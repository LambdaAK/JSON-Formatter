import Debug.Trace
import Data.Function

data Token =
    LBRACE
    | RBRACE
    | LBRACKET
    | RBRACKET
    | QUOTATION
    | STRING String
    | COMMA
    | COLON
    | TRUE
    | FALSE
    | NULL
    | NUMBER Double
    deriving (Show)

isLetter :: Char -> Bool
isLetter c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

lexNumber :: String -> Maybe (Double, String)
lexNumber tokens = do
    (lexedDigits, remainder) <- lexDigits tokens False
    let number = read lexedDigits :: Double
    return (number, remainder)

lexDigits :: String -> Bool -> Maybe (String, String)
-- string is the tokens
-- is weather or not we have seen a decimal point

lexDigits [] _ = Just ("", [])
lexDigits (x:xs) seenDecimal
  | x == '.' = if seenDecimal
                then Nothing
                else do
                    (lexedDigits, remainder) <- lexDigits xs True
                    return (x:lexedDigits, remainder)
  | isDigit x = do
            (lexedDigits, remainder) <- lexDigits xs seenDecimal
            return (x:lexedDigits, remainder)
  | otherwise = return ("", x:xs)


lexString :: String -> Maybe (String, String)
-- first string is the lexed string, second string is the remainder of the input
lexString [] = Nothing
lexString (x : xs)
  | x == '"' = return ("", xs)
  | not (isLetter x) = Nothing
  | otherwise = do
        (lexedString :: String, remainder :: String) <- lexString xs
        return (x:lexedString, remainder)

lexTokens :: String -> Maybe [Token]
lexTokens [] = Just []
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
                    (lexedString :: String, remainder :: String) <- lexString (x:xs)
                    let newToken :: Token = STRING lexedString in do
                        remainderTokens :: [Token] <- lexTokens remainder
                        return (newToken:remainderTokens)
            else if isDigit x
                then do
                    (number :: Double, remainder :: String) <- lexNumber (x:xs)
                    let newToken :: Token = NUMBER number in do
                        remainderTokens :: [Token] <- lexTokens remainder
                        return (newToken:remainderTokens)
                else
                    let newToken :: Token = case x of
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



main :: IO ()
main = do
    "{12.1.}" & lexTokens & print
