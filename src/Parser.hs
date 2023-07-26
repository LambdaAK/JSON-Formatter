{-# LANGUAGE ScopedTypeVariables #-}

module Parser (
    parse
)
where

import AST
import Proc
import Tokens
import Data.List (tail)
import Data.Function


parseNodesSeperatedByCommas :: [Token] -> Proc ([Node], [Token])
parseNodesSeperatedByCommas [] = Err "unexpected end of input while parsing array"
parseNodesSeperatedByCommas tokens = do
    (first :: Node, tokensAfterFirst :: [Token]) <- parse tokens
    case tokensAfterFirst of
        (COMMA : t) -> do -- parse more nodes
            (rest :: [Node], tokensAfterRest :: [Token]) <- parseNodesSeperatedByCommas t
            return (first : rest, tokensAfterRest)
        (RBRACKET : t) -> return ([first], RBRACKET : t) -- finished
        _ -> Err "expected comma or right bracket while parsing array"

parseKeyValuePairsSeperatedByCommas :: [Token] -> Proc ([(String, Node)], [Token])
parseKeyValuePairsSeperatedByCommas [] = undefined


parse :: [Token] -> Proc (Node, [Token])
parse [] = Err "Empty input"
parse tokens =
    -- cases where one token is one node
    case tokens of
        (FALSE : t) -> return (NBool False, t)
        (TRUE : t) -> return (NBool True, t)
        (NULL : t) -> return (NNull, t)
        (NUMBER n : t) -> return (NNumber n, t)
        (QUOTATION : STRING s : QUOTATION : t) -> return (NString s, t)
        (LBRACKET : RBRACKET : t) -> return (NArray [], t)
        (LBRACKET : t) -> do
            -- parse nodes seperated by commas
            (nodes :: [Node], tokensAfterNodes :: [Token]) <- parseNodesSeperatedByCommas t
            -- there is still a right bracket
            case tokensAfterNodes of
                (RBRACKET : t) -> return (NArray nodes, t)
                _ -> Err "expected right bracket while parsing array"

        _ -> tokens & show & Err
