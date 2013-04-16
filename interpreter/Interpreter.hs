import Language.Python.Version2.Parser
import Language.Python.Common.AST
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

data Value = VInt Integer
           | VBool Bool
           deriving Show
    
type Env = Map.Map String Value

evalExpr :: Env -> ExprSpan -> Value
evalExpr e i@Int{} = VInt (int_value i)
evalExpr e i@LongInt{} = VInt (int_value i)
evalExpr e var@Var{} = value
  where Just value = Map.lookup (ident_string . var_ident $ var) e
evalExpr e binop@BinaryOp{} = VInt $ i1 `op` i2
  where VInt i1 = evalExpr e $ left_op_arg binop
        VInt i2 = evalExpr e $ right_op_arg binop
        op = case operator binop of
          Plus{} -> (+)
          Minus{} -> (-)
          Multiply{} -> (*)
evalExpr e unop@UnaryOp{} = VInt $ -i
  where VInt i = evalExpr e $ op_arg unop
        Minus{} = operator unop
        
        
        
        
-- Utils
eval :: String -> Either String Value
eval = either (Left . show) evaluator . flip parseExpr "outer space"
  where evaluator = Right . evalExpr Map.empty . fst