module Parser (parse, Expression(..), Condition(..)) where

import Tokenizer

data Expression = BinOp Operator Expression Expression
                  | Assign String Expression
                  | UnaryOp Operator Expression
                  | Value Int
                  | Var String
                  | Sequence [Expression]
                  | While Condition Expression
                  | If Condition Expression
                  deriving Show

data Condition = Compare Operator Expression Expression
                 | UnaryCond Operator Condition
                 | TRUE
                 | FALSE
                 deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

parse :: [Token] -> [Expression]
parse [] = []
parse toks = let (tree, toks') = expression toks
             in tree:parse toks'

expression :: [Token] -> (Expression, [Token])
expression toks = 
  let
    (termTree, toks') = term toks
  in
    case lookAhead toks' of
      (TokOp op) | elem op [Plus, Minus] -> 
        let
          (exTree, toks'') = expression (accept toks') 
        in
          (BinOp op termTree exTree, toks'')
      TokAssign ->
        case termTree of
          Var str -> 
            let
              (exTree, toks'') = expression (accept toks') 
            in
              (Assign str exTree, toks'')
          _ -> error "Only variables can be assigned to"
      TokIncrement ->
        case termTree of
          Var str ->
            let
              (exTree, toks'') = expression (accept toks')
            in
              (Assign str (BinOp Plus (Var str) exTree), toks'')
          _ -> error "Only variables can be incremented"
      TokDecrement ->
        case termTree of
          Var str ->
            let
              (exTree, toks'') = expression (accept toks')
            in
              (Assign str (BinOp Minus (Var str) exTree), toks'')
          _ -> error "Only variables can be decremented"
      TokMultiplyBy ->
        case termTree of
          Var str ->
            let
              (exTree, toks'') = expression (accept toks')
            in
              (Assign str (BinOp Times (Var str) exTree), toks'')
          _ -> error "Only variables can be multiplied"
      TokDivideBy ->
        case termTree of
          Var str ->
            let
              (exTree, toks'') = expression (accept toks')
            in
              (Assign str (BinOp Div (Var str) exTree), toks'')
          _ -> error "Only variables can be divided"
      TokAddOne ->
        case termTree of
          Var str ->
            (Assign str (BinOp Plus (Var str) (Value 1)), accept toks')
          _ -> error "Only variables can be incremented"
      TokSubtractOne ->
        case termTree of
          Var str ->
            (Assign str (BinOp Minus (Var str) (Value 1)), accept toks')
          _ -> error "Only variables can be incremented"
      _ -> (termTree, toks')

condition :: [Token] -> (Condition, [Token])
condition toks =
  case lookAhead toks of
    TokTrue ->
      (TRUE, accept toks)
    TokFalse ->
      (FALSE, accept toks)
    (TokOp op) ->
      let
        (cond, toks') = condition (accept toks)
      in
        (UnaryCond op cond, toks')
    TokLParen ->
      let
        (cond, toks') = condition (accept toks)
      in
        if lookAhead toks' /= TokRParen
        then error "Missing right parenthesis"
        else (cond, accept toks')
    _ ->
      let
        (exp1, toks') = expression toks
        opToken = lookAhead toks'
        (exp2, toks'') = expression (accept toks')
      in
        case opToken of
          (TokOp op) -> (Compare op exp1 exp2, toks'') 
          _ -> error "Expecting comparison of expressions"

parseSeq :: [Token] -> ([Expression], [Token])
parseSeq [] = error "No right bracket"
parseSeq (TokRBracket:toks') = ([], toks')
parseSeq toks =
  let
    (ex, toks') = expression toks
    (exps, toks'') = parseSeq toks'
  in
    (ex:exps, toks'')


term :: [Token] -> (Expression, [Token])
term toks = 
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = term (accept toks') 
            in (BinOp op facTree termTree, toks'')
         _ -> (facTree, toks')

factor :: [Token] -> (Expression, [Token])
factor toks = 
   case lookAhead toks of
      (TokNum x)     -> (Value x, accept toks)
      (TokIdent str) -> (Var str, accept toks)
      (TokOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = factor (accept toks) 
            in (UnaryOp op facTree, toks')
      TokLParen      -> 
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      TokLBracket ->
        let
          (exps, toks') = parseSeq (accept toks)
        in
          (Sequence exps, toks')
      TokIf ->
        let
          (cond, toks') = condition (accept toks)
          (stmt, toks'') = expression toks'
        in
          (While cond stmt, toks'')
      TokWhile ->
        let
          (cond, toks') = condition (accept toks)
          (stmt, toks'') = expression toks'
        in
          (While cond stmt, toks'')
      _ -> error $ "Parse error on token: " ++ show toks
