module Value where

import qualified Env
import Language.Python.Common.AST (ExprSpan)

data Value = VInt Integer
           | VBool Bool
           | VStr String
           | VNone
           | VCls (Env.Env String Value) [String] (ExprSpan)
           deriving Show

typeName :: Value -> String
typeName (VInt _) = "int"
typeName (VBool _) = "bool"
typeName (VStr _) = "str"
typeName VNone = "NoneType"
