module Value where

import qualified Env
import Heap (Register)
import Language.Python.Common.AST (ExprSpan)

data Value = VInt Integer
           | VBool Bool
           | VStr String
           | VNone
           | VCls (Env.Env String Register) [String] (ExprSpan)
           deriving Show

typeName :: Value -> String
typeName (VInt _) = "int"
typeName (VBool _) = "bool"
typeName (VStr _) = "str"
typeName VNone = "NoneType"
typeName VCls{} = "function"
