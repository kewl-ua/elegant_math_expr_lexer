module Main where

import Lexer (tokenize, Token (..))

main :: IO()
main = do
    let expr = "2 * (x + 3.14)"

    putStrLn $ "Parsing the expression: " ++ expr
    print (tokenize expr)


