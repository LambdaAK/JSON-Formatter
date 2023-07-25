import Language.Haskell.TH (safe)
data Token =
    LBRACE
    | RBRACE
    | LBRACKET
    | RBRACKET
    | QUOTATION
    | STRING String
    deriving (Show)

isLetter :: Char -> Bool
isLetter c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

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
lexTokens (x:xs) =
    if isLetter x
        then do
            (lexedString :: String, remainder :: String) <- lexString (x:xs)
            let newToken :: Token = STRING lexedString in do
                remainderTokens :: [Token] <- lexTokens remainder
                return (newToken:remainderTokens)
        else
            let newToken :: Token = case x of
                    '{' -> LBRACE
                    '}' -> RBRACE
                    '[' -> LBRACKET
                    ']' -> RBRACKET
                    '"' -> QUOTATION
                    _ -> error "Invalid token"
            in do
                remainderTokens <- lexTokens xs
                return (newToken:remainderTokens)



main :: IO ()
main = do
    let a = lexTokens "{\"aa\"}"
    print a
