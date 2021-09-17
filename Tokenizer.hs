module Tokenizer (tokenize, Token(..), Operator(..)) where

import Data.Char

data Operator = Plus | Minus | Times | Div | GreaterThan | LessThan | Equals | Not | LesserOrEqual | GreaterOrEqual
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokLBracket
           | TokRBracket
           | TokIdent String
           | TokNum Int
           | TokIf
           | TokWhile
           | TokFor
           | TokEnd
           | TokTrue
           | TokFalse
           | TokNot
           | TokIncrement
           | TokDecrement
           | TokMultiplyBy
           | TokDivideBy
           | TokSubtractOne
           | TokAddOne
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '>' = GreaterThan
           | c == '<' = LessThan

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | c == '('  = TokLParen : tokenize cs
  | c == ')'  = TokRParen : tokenize cs
  | c == '{'  = TokLBracket : tokenize cs
  | c == '}'  = TokRBracket : tokenize cs
  | elem c "+-*/><=" = getSymbol (c:cs)
  | isDigit c = getNum (c:cs)
  | isAlpha c = getString (c:cs)
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize " ++ [c]


getNum :: String -> [Token]
getNum cs =
  let
    (digs, cs') = span isDigit cs
  in
    TokNum (read digs) : tokenize cs'

getString :: String -> [Token]
getString cs =
  let
    (name, cs') = span isAlphaNum cs
  in
    case () of
      () | name == "while" -> TokWhile : tokenize cs'
         | name == "true" -> TokTrue : tokenize cs'
         | name == "if" -> TokIf : tokenize cs'
         | name == "false" -> TokFalse : tokenize cs'
         | name == "not" -> TokOp Not : tokenize cs'
         | name == "for" -> TokFor : tokenize cs'
         | otherwise -> TokIdent name : tokenize cs'

getSymbol :: String -> [Token]
getSymbol cs =
  let
    (symbols, cs') = span (flip elem $ "+-*/><:=") cs
  in
    case () of
      () | symbols == "=" -> TokAssign : tokenize cs'
         | symbols == "+" -> (TokOp Plus) : tokenize cs'
         | symbols == "-" -> (TokOp Minus) : tokenize cs'
         | symbols == "*" -> (TokOp Times) : tokenize cs'
         | symbols == "/" -> (TokOp Div) : tokenize cs'
         | symbols == ">" -> (TokOp GreaterThan) : tokenize cs'
         | symbols == "<" -> (TokOp LessThan) : tokenize cs'
         | symbols == "==" -> (TokOp Equals) : tokenize cs'
         | symbols == ">=" -> (TokOp GreaterOrEqual) : tokenize cs'
         | symbols == "<=" -> (TokOp LesserOrEqual) : tokenize cs'
         | symbols == "+=" -> TokIncrement : tokenize cs'
         | symbols == "-=" -> TokDecrement : tokenize cs'
         | symbols == "*=" -> TokMultiplyBy : tokenize cs'
         | symbols == "/=" -> TokDivideBy : tokenize cs'
         | symbols == "++" -> TokAddOne : tokenize cs'
         | symbols == "--" -> TokSubtractOne : tokenize cs'
         | otherwise -> error $ "Unknown symbol" ++ symbols

