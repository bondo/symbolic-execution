module Value where

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
