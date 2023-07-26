{-# LANGUAGE ScopedTypeVariables #-}
module Main where

--import Debug.Trace
import Data.Function
import Lexer
import Parser
import Proc
import Tokens
import AST

main :: IO ()
main =
    let s = "[\"a\", false ,true]" in
    let result :: Proc Node = do
            tokens :: [Token] <- lexTokens s
            (node :: Node, _) <- parse tokens
            return node
    in
    case result of
        Suc node -> print node
        Err e -> print e
