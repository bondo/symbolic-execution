module Utils where

import Control.Monad (liftM)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.ParseError (ParseError)
import Language.Python.Common.Pretty (pretty, render)
--import Language.Python.Common.PrettyAST ()
import Language.Python.Version3.Parser (parseExpr, parseStmt, parseModule)

import Names (namesExpr, namesStmts, namesModule)
import Simplifier (simplExpr, evalNameGen)

getNames :: (String -> String -> Either ParseError (a,b)) -> (a -> Set String) -> String -> Set String
getNames parse names = either (const Set.empty) (names . fst) . flip parse "" . (++"\n")

parseNamesInExpr :: String -> Set String
parseNamesInExpr = getNames parseExpr namesExpr

parseNamesInStmt :: String -> Set String
parseNamesInStmt = getNames parseStmt namesStmts

parseNamesInModule :: String -> IO (Set String)
parseNamesInModule fname = getNames parseModule namesModule `liftM` readFile fname

simplifyExpr :: String -> IO ()
simplifyExpr str =  putStrLn $ either (("Error: "++) . show) (prettify . simplify) parsed
  where parsed = parseExpr str ""
        names = parseNamesInExpr str
        simplify (ast, _) = evalNameGen names $ simplExpr ast
        prettify (stmts, expr) = prettyStmts ++ prettyExpr
          where prettyStmts = "Stmts: " ++ concatMap (("\n  "++) . render . pretty) stmts ++ "\n"
                prettyExpr = "Expr: " ++ render (pretty expr)
-- > simplifyExpr "tmp_2 + tmp_6 * (tmp_1 - tmp_9) * tmp_8"
-- Stmts: 
--   tmp_3 = tmp_1 - tmp_9
--   tmp_4 = tmp_6 * tmp_3
--   tmp_5 = tmp_4 * tmp_8
-- Expr: tmp_2 + tmp_5
