module ToString where

import AST
import Data.Function

indent :: Int -> String
indent i = take i (repeat ' ')

-- Int adds indentation
toString :: Node -> Int -> String
toString (NBool b) i = indent i ++ show b
toString (NNumber n) i = indent i ++ show n
toString (NString s) i = indent i ++ show s
toString NNull i = indent i ++ "null"
toString (NArray nodes) i =
    if length nodes == 0 then
        indent i ++ "[]"
    else
    let inside = nodes & map (\x -> toString x (i + 1)) & foldl (\acc x -> acc ++ x ++ ",\n") "" in
    let new_inside = if length inside > 0 then take (length inside - 2) inside else inside in
    indent i ++ "[\n" ++ new_inside ++ "\n" ++ indent i ++ "]"
toString (NObject pairs) i =
    if length pairs == 0 then
        indent i ++ "{}"
    else
    let inside = pairs & map (\(k, v) -> indent (i + 1) ++ show k ++ ": " ++ toString v (i + 1)) & foldl (\acc x -> acc ++ x ++ ",\n") "" in
    let new_inside = if length inside > 0 then take (length inside - 2) inside else inside in
    indent i ++ "{\n" ++ new_inside ++ "\n" ++ indent i ++ "}"
