{-# LANGUAGE PackageImports #-}

module Simplifier where

import "mtl" Control.Monad.State
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()

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

sequenceNameGen :: [NameGen ([a], b)] -> NameGen ([a], [b])
sequenceNameGen []     = return ([], [])
sequenceNameGen (n:ns) = undefined

mkVar :: Ident a -> Expr a
mkVar i = Var i $ ident_annot i

newVar :: a -> NameGen (Expr a)
newVar annot = do name <- freshName
                  return . mkVar $ Ident name annot

mkAssign :: Expr a -> Expr a -> Statement a
mkAssign lhs rhs = Assign [lhs] rhs $ expr_annot lhs

-- Return ([simple statement], Var expression)
simplVar :: Expr a -> NameGen ([Statement a], Expr a)
simplVar e@Var{} = return $ ([], e)
simplVar e = do (stmts, expr) <- simplExpr e
                var <- newVar $ expr_annot e
                return (stmts ++ [mkAssign var expr], var)

-- Return ([simple statement], simple argument)
simplArgument :: Argument a -> NameGen ([Statement a], Argument a)
simplArgument a = do (exprStmts, exprVar) <- simplVar $ arg_expr a
                     return (exprStmts, a{arg_expr = exprVar})

-- Return ([simple statement], simple expression)
simplExpr :: Expr a -> NameGen ([Statement a], Expr a)
simplExpr e@Var{}            = return $ ([], e)
simplExpr e@Int{}            = return $ ([], e)
simplExpr e@LongInt{}        = return $ ([], e)
simplExpr e@Float{}          = return $ ([], e)
simplExpr e@Imaginary{}      = return $ ([], e)
simplExpr e@Bool{}           = return $ ([], e)
simplExpr e@None{}           = return $ ([], e)
simplExpr e@Ellipsis{}       = return $ ([], e)
simplExpr e@ByteStrings{}    = return $ ([], e)
simplExpr e@Strings{}        = return $ ([], e)
simplExpr e@UnicodeStrings{} = return $ ([], e)
simplExpr e@Call{} = do (funStmts, funVar) <- simplVar $ call_fun e
                        (argStmts, argVars) <- sequenceNameGen . map simplArgument $ call_args e
                        return (funStmts ++ argStmts, e{call_fun = funVar, call_args = argVars})
-- Subscript
-- SlicedExpr
-- CondExpr
simplExpr e@BinaryOp{} = do (leftStmts, leftVar) <- simplVar $ left_op_arg e
                            (rightStmts, rightVar) <- simplVar $ right_op_arg e
                            return (leftStmts ++ rightStmts, e{left_op_arg = leftVar, right_op_arg = rightVar})
simplExpr e@UnaryOp{} = do (stmts, var) <- simplVar $ op_arg e
                           return (stmts, e{op_arg = var})
-- Lambda
simplExpr e@Tuple{} = do (stmts, vars) <- sequenceNameGen . map simplVar $ tuple_exprs e
                         return (stmts, e{tuple_exprs = vars})
-- Yield
-- Generator
-- ListComp
simplExpr e@List{} = do (stmts, vars) <- sequenceNameGen . map simplVar $ list_exprs e
                        return (stmts, e{list_exprs = vars})
-- Dictionary
-- DictComp
simplExpr e@Set{} = do (stmts, vars) <- sequenceNameGen . map simplVar $ set_exprs e
                       return (stmts, e{set_exprs = vars})
-- SetComp
-- Starred
simplExpr e@Paren{} = simplExpr $ paren_expr e
-- StringConversion
simplExpr e = error $ "Simplifier.simplExpr called on unsupported expression: " ++ render (pretty e)
