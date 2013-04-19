module Interpreter (Env, evalExpr, run, eval, exec) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Data.List (foldl1')

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()

import Magic
import StringParser
import Value
import qualified Env
import qualified Heap
import Heap (Register)

type Env = Env.Env String Register
type HeapM = Heap.HeapM Value
type InterpreterM = EitherT String HeapM

evalExpr :: Env -> ExprSpan -> InterpreterM Value
evalExpr _ i@Int{}      = return . VInt $ int_value i
evalExpr _ i@LongInt{}  = return . VInt $ int_value i
evalExpr _ b@Bool{}     = return . VBool $ bool_value b
evalExpr _ None{}       = return VNone
evalExpr _ ss@Strings{} = do
  s <- hoistEither $ foldl1' (liftM2 (++)) strs
  return $ VStr s
  where strs = map parseString $ strings_strings ss
evalExpr e var@Var{} = do
  cell  <- maybe err return $ Env.lookup str e
  value <- lift $ Heap.lookup cell
  maybe err2 return value
  where str  = ident_string . var_ident $ var
        err  = left $ "The variable " ++ str ++ " is not defined."
        err2 = left $ "The variable " ++ str ++ " points to uninitialized memory."
evalExpr e binop@BinaryOp{} = do
  v1 <- evalExpr e $ left_op_arg binop
  v2 <- evalExpr e $ right_op_arg binop
  op <- hoistEither $ getBinOp $ operator binop
  hoistEither $ v1 `op` v2
evalExpr e unop@UnaryOp{} = do
  i  <- evalExpr e $ op_arg unop
  op <- hoistEither $ getUnOp $ operator unop
  hoistEither $ op i
evalExpr e par@Paren{} = evalExpr e $ paren_expr par
evalExpr e cond@CondExpr{} = do
  c <- evalExpr e $ ce_condition cond
  evalExpr e $ if getBool c
               then ce_true_branch cond
               else ce_false_branch cond
evalExpr e lam@Lambda{} = do
  args <- hoistEither $ mapM name $ lambda_args lam
  return $ VCls e args $ lambda_body lam
  where name param@Param{} = Right . ident_string . param_name $ param
        name p = Left $ "Unsupported parameter syntax: " ++ show p
evalExpr e call@Call{} = do
  fun  <- evalExpr e $ call_fun call
  args <- evalExprList e =<< (hoistEither $ mapM unpack $ call_args call)
  apply fun args
  where unpack arg@ArgExpr{} = Right $ arg_expr arg
        unpack arg = Left $ "Unsupported argument syntax: " ++ show arg
evalExpr _ expr = left $ "Evaluation of expression not implemented: " ++ render (pretty expr)

evalExprList :: Env -> [ExprSpan] -> InterpreterM [Value]
evalExprList = mapM . evalExpr
  
apply :: Value -> [Value] -> InterpreterM Value
apply fun args = do
  (e, params, body) <- ecls
  argRegisters  <- lift $ mapM Heap.malloc args
  assoc <- if length params == length argRegisters
           then return $ zip params argRegisters
           else left "Wrong number of arguments, yo!"
  evalExpr (Env.extend e assoc) body
  where ecls = case fun of
          VCls e params body -> return (e, params, body)
          _ -> left "Trying to apply non-function"

getBool :: Value -> Bool
getBool (VInt 0)  = False
getBool (VInt _)  = True
getBool (VBool b) = b
getBool (VStr "") = False
getBool (VStr _)  = True
getBool VNone     = False
getBool VCls{}    = True

run :: InterpreterM a -> (Either String a, Heap.Heap Value)
run = Heap.run . runEitherT

eval :: InterpreterM a -> Either String a
eval = Heap.eval . runEitherT

exec :: InterpreterM a -> Heap.Heap Value
exec = Heap.exec . runEitherT
