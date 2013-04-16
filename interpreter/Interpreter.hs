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

typeName :: Value -> String
typeName (VInt _) = "int"
typeName (VBool _) = "bool"
typeName (VStr _) = "str"
typeName VNone = "NoneType"

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

getBinOp :: Op annot -> Either String (Value -> Value -> Either String Value)
getBinOp Plus{} = Right __add__
getBinOp Minus{} = Right __sub__
getBinOp Multiply{} = Right __mul__
getBinOp op = Left $ "Binary operator '" ++ render (pretty op) ++ "' not implemented."

getUnOp :: Op annot -> Either String (Value -> Either String Value)
getUnOp Minus{} = Right __neg__
getUnOp other = Left $ "Uniary operator '" ++ render (pretty other) ++ "' not implemented."

__add__ :: Value -> Value -> Either String Value
__add__ v1@(VInt i1) = int__add__
  where int__add__ (VInt i2) = Right . VInt $ i1 + i2
        int__add__ (VBool b) = Right . VInt $ i1 + if b then 1 else 0
        int__add__ v2 = Left $ operandTypeError "+" v1 v2
__add__ v1@(VBool b) = bool__add__
  where i1 = if b then 1 else 0
        bool__add__ (VInt i2) = Right . VInt $ i1 + i2
        bool__add__ (VBool b) = Right . VInt $ i1 + if b then 1 else 0
        bool__add__ v2 = Left $ operandTypeError "+" v1 v2
__add__ v1@(VStr s1) = str__add__
  where str__add__ (VStr s2) = Right . VStr $ s1 ++ s2
        str__add__ v2 = Left $ operandTypeError "+" v1 v2
__add__ v1 = Left . operandTypeError "+" v1

__sub__ :: Value -> Value -> Either String Value
__sub__ v1@(VInt i1) = int__sub__
  where int__sub__ (VInt i2) = Right . VInt $ i1 - i2
        int__sub__ (VBool b) = Right . VInt $ i1 - if b then 1 else 0
        int__sub__ v2 = Left $ operandTypeError "-" v1 v2
__sub__ v1@(VBool b) = bool__sub__
  where i1 = if b then 1 else 0
        bool__sub__ (VInt i2) = Right . VInt $ i1 - i2
        bool__sub__ (VBool b) = Right . VInt $ i1 - if b then 1 else 0
        bool__sub__ v2 = Left $ operandTypeError "-" v1 v2
__sub__ v1 = Left . operandTypeError "-" v1

__mul__ :: Value -> Value -> Either String Value
__mul__ v1@(VInt i1) = int__mul__
  where int__mul__ (VInt i2) = Right . VInt $ i1 * i2
        int__mul__ (VBool b) = Right . VInt $ if b then i1 else 0
        int__mul__ v2 = Left $ operandTypeError "*" v1 v2
__mul__ v1@(VBool b1) = bool__mul__
  where bool__mul__ (VInt i2) = Right . VInt $ if b1 then i2 else 0
        bool__mul__ (VBool b2) = Right . VInt $ if b1 && b2 then 1 else 0
        bool__mul__ v2 = Left $ operandTypeError "*" v1 v2

operandTypeError :: String -> Value -> Value -> String
operandTypeError op v1 v2 = "TypeError: unsupported operand type(s) for "
                            ++ op ++ ": "
                            ++ "'" ++ typeName v1 ++ "'"
                            ++ " and "
                            ++ "'" ++ typeName v2 ++ "'"
                
__neg__ :: Value -> Either String Value
__neg__ (VInt i) = Right . VInt $ -i
__neg__ (VBool b) = Right . VInt $ if b then -1 else 0
__neg__ v = Left $ "TypeError: bad operand type for unary -: '"
            ++ typeName v ++ "'"

-- Utils
eval :: String -> Either String Value
eval = either (Left . show) evaluator . flip parseExpr "outer space"
  where evaluator = evalExpr emptyEnv . fst

evalIO :: String -> IO ()
evalIO = putStrLn . either ("Error: "++) (("Result: "++) . show) . eval