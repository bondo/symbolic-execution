module Interpreter (evalExpr) where

import Language.Python.Version3.Parser (parseExpr)
import Language.Python.Common.AST
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.Pretty (pretty)

import Control.Monad (liftM2)
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.PrettyPrint (render)

import Value
import Magic
import StringParser

type Env = Map.Map String Value

emptyEnv = Map.empty

evalExpr :: Env -> ExprSpan -> Either String Value
evalExpr e i@Int{}     = Right . VInt $ int_value i
evalExpr e i@LongInt{} = Right . VInt $ int_value i
evalExpr e b@Bool{}    = Right . VBool $ bool_value b
evalExpr e None{}      = Right VNone
evalExpr e s@Strings{} = foldl1' (liftM2 (++)) vals >>= return . VStr
  where vals = map parseString $ strings_strings s
evalExpr e var@Var{}   = maybe err Right $ Map.lookup str e
  where str = ident_string . var_ident $ var
        err = Left $ "The variable " ++ str ++ " is not defined."
evalExpr e binop@BinaryOp{} = do
  v1 <- evalExpr e $ left_op_arg binop
  v2 <- evalExpr e $ right_op_arg binop
  op  <- getBinOp $ operator binop
  v1 `op` v2
evalExpr e unop@UnaryOp{} = do
  i  <- evalExpr e $ op_arg unop
  op <- getUnOp $ operator unop
  op i
evalExpr e par@Paren{} = evalExpr e $ paren_expr par
evalExpr e cond@CondExpr{} = do
  c <- evalExpr e $ ce_condition cond
  evalExpr e $ if getBool c
               then ce_true_branch cond
               else ce_false_branch cond

getBool :: Value -> Bool
getBool (VInt 0)  = False
getBool (VInt _)  = True
getBool (VBool b) = b
getBool (VStr "") = False
getBool (VStr _)  = True
getBool VNone     = False


-- Utils
eval :: String -> Either String Value
eval = either (Left . show) evaluator . flip parseExpr "outer space"
  where evaluator = evalExpr emptyEnv . fst

evalIO :: String -> IO ()
evalIO = putStrLn . either ("Error: "++) (("Result: "++) . show) . eval