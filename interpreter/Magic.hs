module Magic (getBinOp, getUnOp) where

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()

import Value

getBinOp :: Op a -> Either String (Value -> Value -> Either String Value)
getBinOp Plus{} = Right __add__
getBinOp Minus{} = Right __sub__
getBinOp Multiply{} = Right __mul__
getBinOp LessThan{} = Right __lt__
getBinOp op = Left $ "Binary operator '" ++ render (pretty op) ++ "' not implemented."

getUnOp :: Op annot -> Either String (Value -> Either String Value)
getUnOp Minus{} = Right __neg__
getUnOp other = Left $ "Uniary operator '" ++ render (pretty other) ++ "' not implemented."

__add__ :: Value -> Value -> Either String Value
__add__ v1@(VInt i1) = int__add__
  where int__add__ (VInt i2)  = Right . VInt $ i1 + i2
        int__add__ (VBool b2) = Right . VInt $ i1 + if b2 then 1 else 0
        int__add__ v2         = Left $ operandTypeError "+" v1 v2
__add__ v1@(VBool b1) = bool__add__
  where i1 = if b1 then 1 else 0
        bool__add__ (VInt i2)  = Right . VInt $ i1 + i2
        bool__add__ (VBool b2) = Right . VInt $ i1 + if b2 then 1 else 0
        bool__add__ v2         = Left $ operandTypeError "+" v1 v2
__add__ v1@(VStr s1) = str__add__
  where str__add__ (VStr s2) = Right . VStr $ s1 ++ s2
        str__add__ v2        = Left $ operandTypeError "+" v1 v2
__add__ v1 = Left . operandTypeError "+" v1

__sub__ :: Value -> Value -> Either String Value
__sub__ v1@(VInt i1) = int__sub__
  where int__sub__ (VInt i2)  = Right . VInt $ i1 - i2
        int__sub__ (VBool b2) = Right . VInt $ i1 - if b2 then 1 else 0
        int__sub__ v2         = Left $ operandTypeError "-" v1 v2
__sub__ v1@(VBool b1) = bool__sub__
  where i1 = if b1 then 1 else 0
        bool__sub__ (VInt i2)  = Right . VInt $ i1 - i2
        bool__sub__ (VBool b2) = Right . VInt $ i1 - if b2 then 1 else 0
        bool__sub__ v2         = Left $ operandTypeError "-" v1 v2
__sub__ v1 = Left . operandTypeError "-" v1

__mul__ :: Value -> Value -> Either String Value
__mul__ v1@(VInt i1) = int__mul__
  where int__mul__ (VInt i2)  = Right . VInt $ i1 * i2
        int__mul__ (VBool b2) = Right . VInt $ if b2 then i1 else 0
        int__mul__ v2         = Left $ operandTypeError "*" v1 v2
__mul__ v1@(VBool b1) = bool__mul__
  where bool__mul__ (VInt i2)  = Right . VInt $ if b1 then i2 else 0
        bool__mul__ (VBool b2) = Right . VInt $ if b1 && b2 then 1 else 0
        bool__mul__ v2         = Left $ operandTypeError "*" v1 v2
__mul__ v1 = Left . operandTypeError "*" v1

__lt__ :: Value -> Value -> Either String Value
__lt__ v1@(VInt i1) = int__lt__
  where int__lt__ (VInt i2)  = Right . VBool $ i1 < i2
        int__lt__ (VBool b2) = Right . VBool $ if b2
                                              then i1 < 1
                                              else i1 < 0
        int__lt__ v2 = Left $ operandTypeError "<" v1 v2
-- TODO: __lt__ for bools and strings
__lt__ v1 = Left . operandTypeError "<" v1

operandTypeError :: String -> Value -> Value -> String
operandTypeError op v1 v2 = "TypeError: unsupported operand type(s) for "
                            ++ op ++ ": " ++ name v1 ++ " and " ++ name v2
  where name v = "'" ++ typeName v ++ "'"
                
__neg__ :: Value -> Either String Value
__neg__ (VInt i) = Right . VInt $ -i
__neg__ (VBool b) = Right . VInt $ if b then -1 else 0
__neg__ v = Left $ "TypeError: bad operand type for unary -: '"
            ++ typeName v ++ "'"
