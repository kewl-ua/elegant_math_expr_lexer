module Lexer where

import Data.Char (isDigit, isAlpha, isSpace)

-- Data types for tokens
data Token
    = TokNumber Double  -- Numbers: 32, 44
    | TokIdent String   -- Variables / Functions: x, y, sin
    | TokPlus           -- +
    | TokMinus          -- -
    | TokStar           -- *
    | TokSlash          -- /
    | TokPower          -- ^
    | TokLParen         -- (
    | TokRParen         -- )
    deriving (Show, Eq)



tokenize :: String -> Either String [Token]
tokenize [] = Right []

-- Ignoring spaces
tokenize (c:cs) | isSpace c = tokenize cs

-- One symboled operators
tokenize ('+':cs) = (TokPlus :) <$> tokenize cs
tokenize ('-':cs) = (TokMinus :) <$> tokenize cs
tokenize ('*':cs) = (TokStar :) <$> tokenize cs
tokenize ('/':cs) = (TokSlash :) <$> tokenize cs
tokenize ('^':cs) = (TokPower :) <$> tokenize cs
tokenize ('(':cs) = (TokLParen :) <$> tokenize cs
tokenize (')':cs) = (TokRParen :) <$> tokenize cs

-- Numbers (int and float)
tokenize str@(c:_) | isDigit c =
    let (numStr, rest) = span(\x -> isDigit x || x == '.') str
    in case reads numStr :: [(Double, String)] of
        [(val, "")] -> (TokNumber val :) <$> tokenize rest
        _           -> Left $ "Incorrect number: " ++ numStr

-- Identificators (variables, functions, qrt, sin etc)
tokenize str@(c:_) | isAlpha c =
    let (ident, rest) = span isAlpha str
    in (TokIdent ident :) <$> tokenize rest

-- Unknown symbol
tokenize (c:_) = Left $ "Unknown character: " ++ [c]



