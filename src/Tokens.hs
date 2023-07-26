module Tokens (
    Token(..),    
)
where
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