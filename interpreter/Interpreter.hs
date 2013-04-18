module Interpreter (evalExpr) where

import Control.Monad (liftM, liftM2)
import Data.List (foldl1')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Language.Python.Common.AST
import Language.Python.Version3.Parser (parseExpr)

import Magic
import StringParser
import Value
import qualified Env

type Env = Env.Env String Value

evalExpr :: Env -> ExprSpan -> Either String Value
evalExpr e i@Int{}     = Right . VInt $ int_value i
evalExpr e i@LongInt{} = Right . VInt $ int_value i
evalExpr e b@Bool{}    = Right . VBool $ bool_value b
evalExpr e None{}      = Right VNone
evalExpr e s@Strings{} = VStr `liftM` foldl1' (liftM2 (++)) strs
  where strs = map parseString $ strings_strings s
evalExpr e var@Var{}   = maybe err Right $ Env.lookup str e
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
evalExpr e lam@Lambda{} = Right $ VCls e (map name (lambda_args lam)) (lambda_body lam)
  where name param@Param{} = ident_string . param_name $ param
evalExpr e call@Call{} = do
  fun <- evalExpr e (call_fun call)
  args <- foldl (liftM2 . flip $ (:)) (Right []) $ map (evalExpr e . unpack) $ call_args call
  apply fun args
  -- TODO: Other cases / error message
  where unpack arg@ArgExpr{} = arg_expr arg
  
apply :: Value -> [Value] -> Either String Value
apply fun args = do
  (e, params, body) <- ecls
  assoc <- if length params == length args
           then Right $ zip params args
           else Left "Wrong number of arguments, yo!"
  evalExpr (Env.extend e assoc) body
  where ecls = case fun of
          VCls e params body -> Right (e, params, body)
          _ -> Left "Trying to apply non-function"

getBool :: Value -> Bool
getBool (VInt 0)  = False
getBool (VInt _)  = True
getBool (VBool b) = b
getBool (VStr "") = False
getBool (VStr _)  = True
getBool VNone     = False


-- Utils
evalWith :: Env -> String -> Either String Value
evalWith e = either (Left . show) evaluator . flip parseExpr "outer space"
  where evaluator = evalExpr e . fst
        
eval :: String -> Either String Value
eval = evalWith Env.emptyEnv

evalWithIO :: Env -> String -> IO ()
evalWithIO e = putStrLn . either ("Error: "++) (("Result: "++) . show) . evalWith e

evalIO :: String -> IO ()
evalIO = evalWithIO Env.emptyEnv
