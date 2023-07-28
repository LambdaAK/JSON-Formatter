{-# LANGUAGE ScopedTypeVariables #-}
module Main where

--import Debug.Trace
import Data.Function
import Lexer
import Parser
import Proc
import Tokens
import AST
import ToString

main :: IO ()
main =
    let s = "{\"b\": null, \"a\": true}" in
    let result :: Proc Node = do
            tokens :: [Token] <- lexTokens s
            (node :: Node, _) <- parse tokens
            return node
    in
    case result of
        Suc node -> toString node 0 & putStrLn
        Err e -> print e
