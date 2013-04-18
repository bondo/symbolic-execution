module Interpreter (evalExpr) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Data.List (foldl1', foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()
import Language.Python.Version3.Parser (parseExpr)

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
evalExpr e i@Int{}     = return $ VInt $ int_value i
evalExpr e i@LongInt{} = return $ VInt $ int_value i
evalExpr e b@Bool{}    = return $ VBool $ bool_value b
evalExpr e None{}      = return VNone
evalExpr e s@Strings{} = do
  s <- hoistEither $ foldl1' (liftM2 (++)) strs
  return $ VStr s
  where strs = map parseString $ strings_strings s
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
evalExpr e lam@Lambda{} = return $ VCls e (map name $ lambda_args lam) $ lambda_body lam
  -- TODO: Other cases / error message
  where name param@Param{} = ident_string . param_name $ param
evalExpr e call@Call{} = do
  fun  <- evalExpr e $ call_fun call
  args <- evalExprList e $ map unpack $ call_args call
  apply fun args
  -- TODO: Other cases / error message
  where unpack arg@ArgExpr{} = arg_expr arg
evalExpr _ exp = left $ "Evaluation of expression not implemented: " ++ render (pretty exp)

evalExprList :: Env -> [ExprSpan] -> InterpreterM [Value]
evalExprList e = mapM $ evalExpr e
  
apply :: Value -> [Value] -> InterpreterM Value
apply fun args = do
  (e, params, body) <- ecls
  args  <- lift $ mapM Heap.malloc args
  assoc <- if length params == length args
           then return $ zip params args
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

run :: InterpreterM a ->  (Either String a, Heap.Heap Value)
run = Heap.run . runEitherT

exec :: InterpreterM a ->  Either String a
exec = Heap.exec . runEitherT

-- Utils
evalWith :: Env -> String -> Either String Value
evalWith e = either (Left . show) (exec . evalExpr e . fst) . flip parseExpr ""
        
eval :: String -> Either String Value
eval = evalWith Env.emptyEnv

evalWithIO :: Env -> String -> IO ()
evalWithIO e = putStrLn . either ("Error: "++) (("Result: "++) . show) . evalWith e

evalIO :: String -> IO ()
evalIO = evalWithIO Env.emptyEnv
