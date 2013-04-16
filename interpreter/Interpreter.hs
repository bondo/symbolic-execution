import Language.Python.Version3.Parser (parseExpr)
import Language.Python.Common.AST
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.Pretty (pretty)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.PrettyPrint (render)

data Value = VInt Integer
           | VBool Bool
           | VStr String
           | VNone
           deriving Show
    
type Env = Map.Map String Value

emptyEnv = Map.empty

evalExpr :: Env -> ExprSpan -> Either String Value
evalExpr e i@Int{}     = Right . VInt $ int_value i
evalExpr e i@LongInt{} = Right . VInt $ int_value i
evalExpr e b@Bool{}    = Right . VBool $ bool_value b
evalExpr e None{}      = Right VNone
evalExpr e s@Strings{} = Right . VStr . concat $ strings_strings s
evalExpr e var@Var{}   = maybe err Right $ Map.lookup str e
  where str = ident_string . var_ident $ var
        err = Left $ "The variable " ++ str ++ " is not defined."
evalExpr e binop@BinaryOp{} = do
  i1e <- evalExpr e $ left_op_arg binop
  i2e <- evalExpr e $ right_op_arg binop
  i1  <- getInt i1e
  i2  <- getInt i2e
  op  <- eop
  return . VInt $ i1 `op` i2
  where eop = case operator binop of
          Plus{} -> Right (+)
          Minus{} -> Right (-)
          Multiply{} -> Right (*)
          other -> Left $ "Binary operation '" ++ render (pretty other) ++ "' not implemented."
evalExpr e unop@UnaryOp{} = do
  i  <- evalExpr e $ op_arg unop
  op i
  where op i = case operator unop of
          Minus{} -> either Left (Right . VInt . negate) $ getInt i
          other -> Left $ "Uniary operator '" ++ render (pretty other) ++ "' not implemented."
evalExpr e par@Paren{} = evalExpr e $ paren_expr par
evalExpr e cond@CondExpr{} = do
  c <- evalExpr e $ ce_condition cond
  evalExpr e $ if getBool c
               then ce_true_branch cond
               else ce_false_branch cond

getInt :: Value -> Either String Integer
getInt (VInt i)      = Right i
getInt (VBool True)  = Right 1
getInt (VBool False) = Right 0
getInt (VStr _)      = Left "'Str' cannot be used as 'Int'"
getInt VNone         = Left "'None' cannot be used as 'Int'"

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