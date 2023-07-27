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
    show (NArray nodes) = 
        let inside = nodes & map show & foldl (\acc x -> acc ++ x ++ ", ") "" in
        -- remove the last comma and space
        let new_inside = if length inside > 0 then take (length inside - 2) inside else inside in
        "[" ++ new_inside ++ "]"
        
    show (NObject pairs) = "{" ++ (pairs & map (\(k, v) -> k ++ ": " ++ show v) & foldl (\acc x -> acc ++ ", " ++ x) "") ++ "}"

