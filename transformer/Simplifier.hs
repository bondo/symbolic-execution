{-# LANGUAGE PackageImports #-}

module Simplifier where

import Control.Monad (liftM)
import "mtl" Control.Monad.State
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.AST

import Names (namesExpr, namesStmts, namesModule)

type NameGenCounter = State Int
type NameGen = StateT (Set String) NameGenCounter

namePrefix :: String
namePrefix = "tmp_"

nextCount :: NameGen Int
nextCount = lift $ modify (+1) >> get

freshName :: NameGen String
freshName = do name <- ((namePrefix++) . show) `liftM` nextCount
               set  <- get
               if name `Set.member` set
                 then freshName
                 else put (name `Set.insert` set) >> return name

filterPrefix :: String -> Set String -> Set String
filterPrefix = Set.filter . isPrefixOf
-- > filterPrefix "ab" $ Set.fromList ["abc","aba","cba","acb","abb"]
-- fromList ["aba","abb","abc"]

evalNameGen :: Set String -> NameGen a -> a
evalNameGen ss ng = fst $ evalState (runStateT ng $ filterPrefix namePrefix ss) 0
-- > evalNameGen (Set.fromList ["tmp_2","tmp_4"]) . sequence . take 5 $ repeat freshName
-- ["tmp_1","tmp_3","tmp_5","tmp_6","tmp_7"]

-- Return ([simple statement], variable)
simplExpr :: Expr a -> ([Statement b], Expr c)
simplExpr = undefined
