module Utils where

import Value (Value)
import qualified Env
import qualified Interpreter as I
import Interpreter (Env, evalExpr)
import Language.Python.Version3.Parser (parseExpr)


evalWith :: Env -> String -> Either String Value
evalWith e = either (Left . show) (I.eval . evalExpr e . fst) . flip parseExpr ""
        
eval :: String -> Either String Value
eval = evalWith Env.emptyEnv

evalWithIO :: Env -> String -> IO ()
evalWithIO e = putStrLn . either ("Error: "++) (("Result: "++) . show) . evalWith e

evalIO :: String -> IO ()
evalIO = evalWithIO Env.emptyEnv
