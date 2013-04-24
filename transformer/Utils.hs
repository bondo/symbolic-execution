module Utils where

import Control.Monad (liftM)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.ParseError (ParseError)
import Language.Python.Version3.Parser (parseExpr, parseStmt, parseModule)

import Names (namesExpr, namesStmts, namesModule)

getNames :: (String -> String -> Either ParseError (a,b)) -> (a -> Set String) -> String -> Set String
getNames parse names = either (const Set.empty) (names . fst) . flip parse "" . (++"\n")

parseNamesInExpr :: String -> Set String
parseNamesInExpr = getNames parseExpr namesExpr

parseNamesInStmt :: String -> Set String
parseNamesInStmt = getNames parseStmt namesStmts

parseNamesInModule :: String -> IO (Set String)
parseNamesInModule fname = getNames parseModule namesModule `liftM` readFile fname
