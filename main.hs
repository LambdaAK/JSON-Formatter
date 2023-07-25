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

lexString :: String -> (String, String)
-- first string is the lexed string, second string is the remainder of the input
lexString [] = ("", [])
lexString (x:xs) =
    if isLetter x
        then
            let (lexedString, remainder) = lexString xs in
                (x:lexedString, remainder)
        else
            ("", x:xs)


lexTokens :: String -> Maybe [Token]
lexTokens [] = Just []
lexTokens (x:xs) =
    if isLetter x
        then do
            let (lexedString :: String, remainder :: String) = lexString (x:xs)
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
    let a = lexTokens "{aa}"
    print a
