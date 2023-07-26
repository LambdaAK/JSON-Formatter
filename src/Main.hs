module Main where

--import Debug.Trace
import Data.Function
import Lexer

main :: IO ()
main = do
    "{12.12345}" & lexTokens & print
