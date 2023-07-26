module AST (
    Node(..)
)
where


import Data.Function

data Node =
    NBool Bool
    | NNull
    | NNumber Double
    | NString String
    | NArray [Node]
    | NObject [(String, Node)]


instance Show Node where
    show (NBool b) = "Bool (" ++ (if b then "true" else "false") ++ ")"
    show NNull = "null"
    show (NNumber n) = "Number (" ++ show n ++ ")"
    show (NString s) = "String (" ++ s ++ ")"
    show (NArray nodes) = "[" ++ (nodes & map show & foldl (\acc x -> acc ++ ", " ++ x) "") ++ "]"
    show (NObject pairs) = "{" ++ (pairs & map (\(k, v) -> k ++ ": " ++ show v) & foldl (\acc x -> acc ++ ", " ++ x) "") ++ "}"

