module AST (
    AST(..)
)
where

data Node =
    NBool Bool
    | NNull
    | NNumber Double
    | NString String
    | NArray [Node]
    | NObject [(String, Node)]

