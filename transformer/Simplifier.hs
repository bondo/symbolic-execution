module Simplifier where

import Data.Set (toList)

--import Language.Python.Common.AST
--import Language.Python.Common.Pretty (pretty, render)
--import Language.Python.Common.PrettyAST ()
import Language.Python.Version3.Parser (parseExpr, parseStmt, parseModule)

import Names (namesExpr, namesStmts, namesModule)



-- Utils

printEither :: (Show e, Show a) => Either e a -> IO ()
printEither (Left e)  = putStrLn $ "Error: " ++ show e
printEither (Right a) = putStrLn $ "Result: " ++ show a

mapRight :: (a -> b) -> Either e a -> Either e b
mapRight _ (Left e)  = Left e
mapRight f (Right a) = Right $ f a

namesInExpr :: String -> IO ()
namesInExpr = printEither . mapRight (toList . namesExpr . fst) . flip parseExpr ""
-- > namesInExpr "a + b / c == a"
-- Result: ["a","b","c"]

namesInStmt :: String -> IO ()
namesInStmt = printEither . mapRight (toList . namesStmts . fst) . flip parseStmt "" . (++"\n")
-- > namesInStmt "if (a > b):\n b += c\nelse:\n d -= b"
-- Result: ["a","b","c","d"]

namesInModule :: String -> IO ()
namesInModule fname =
  readFile fname >>= printEither . mapRight (toList . namesModule . fst) . flip parseModule "" . (++"\n")
-- > namesInModule "../interpreter/tests/simple1.py"
-- Result: ["y"]
