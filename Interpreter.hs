import Parser
import Tokenizer
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Data.List

type Dict = Map.Map String Int

evaluate :: Expression -> State Dict Int

evaluate (Value n) = return n

evaluate (Var x) =
  state (\s ->
    ((Map.!) s x, s))

evaluate (UnaryOp op ex) =
  let
    ops = [(Minus, (*(-1))), (Plus, id)]
  in
    do
      x <- evaluate ex
      return ((fromJust (lookup op ops)) x)


evaluate (BinOp op exp1 exp2) =
  let
    ops = [(Plus, (+)), (Minus, (-)), (Times, (*)), (Div, div)]
  in
    do
      x <- evaluate exp1
      y <- evaluate exp2
      return ((fromJust (lookup op ops)) x y)

evaluate (Assign x ex) =
  do
    val <- evaluate ex
    state (\s ->
      (val, Map.insert x val s))

evaluate (Sequence []) = return 0
evaluate (Sequence (ex:exs)) =
  do
    s <- get
    evaluate ex
    evaluate (Sequence exs)

evaluate (While cond stmt) =
  do
    b <- evaluateCond cond
    if b
      then
        do
          evaluate stmt
          evaluate (While cond stmt)
      else
        return 0

evaluate (If cond stmt) =
  do
    b <- evaluateCond cond
    if b
      then
        do
          evaluate stmt
      else
        return 0
        
evaluateCond :: Condition -> State Dict Bool

evaluateCond TRUE = return True
evaluateCond FALSE = return False

evaluateCond (UnaryCond op cond) =
  let
    ops = [(Not, not)]
  in
    do
      b <- evaluateCond cond
      return ((fromJust (lookup op ops)) b)

evaluateCond (Compare op exp1 exp2) =
  let
    ops = [(GreaterThan, (>)), (LessThan, (<)), (Equals, (==)), (GreaterOrEqual, (>=)), (LesserOrEqual, (<=))]
  in
    do
      x <- evaluate exp1
      y <- evaluate exp2
      return ((fromJust (lookup op ops)) x y)

run :: String -> Dict
run program =
  let
    toks = tokenize program
    exps = parse toks
  in
    snd $ runState (runProgram exps) Map.empty

runProgram :: [Expression] -> State Dict ()
runProgram [] = return ()
runProgram (ex:exps) =
  do
    evaluate ex
    runProgram exps

stateToString :: Dict -> String
stateToString dict =
  intercalate ['\n'] [k ++ ": " ++ (show v) | (k, v) <- Map.toList dict]

main = do
  program <- getContents
  (putStrLn . stateToString . run) program
