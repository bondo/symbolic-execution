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
sequenceNameGen (n:ns) = do (as, b) <- n
                            (as', bs) <- sequenceNameGen ns
                            return (as ++ as', b : bs)

mapNameGen :: (a -> NameGen ([b], c)) -> [a] -> NameGen ([b], [c])
mapNameGen f = sequenceNameGen . map f

mkVar :: Ident a -> Expr a
mkVar i = Var i $ ident_annot i

newVar :: a -> NameGen (Expr a)
newVar a = do name <- freshName
              return . mkVar $ Ident name a

mkAssign :: Expr a -> Expr a -> Statement a
mkAssign lhs rhs = Assign [lhs] rhs $ expr_annot lhs

-- Return ([simple statement], Var expression)
simplVar :: Expr a -> NameGen ([Statement a], Expr a)
simplVar e@Var{} = return ([], e)
simplVar e = do (stmts, expr) <- simplExpr e
                var <- newVar $ expr_annot e
                return (stmts ++ [mkAssign var expr], var)

-- Return ([simple statement], simple argument)
simplArgument :: Argument a -> NameGen ([Statement a], Argument a)
simplArgument a = do (exprStmts, exprVar) <- simplVar $ arg_expr a
                     return (exprStmts, a{arg_expr = exprVar})

-- Return ([simple statement], simple expression)
simplExpr :: Expr a -> NameGen ([Statement a], Expr a)
simplExpr e@Var{}            = return ([], e)
simplExpr e@Int{}            = return ([], e)
simplExpr e@LongInt{}        = return ([], e)
simplExpr e@Float{}          = return ([], e)
simplExpr e@Imaginary{}      = return ([], e)
simplExpr e@Bool{}           = return ([], e)
simplExpr e@None{}           = return ([], e)
simplExpr e@Ellipsis{}       = return ([], e)
simplExpr e@ByteStrings{}    = return ([], e)
simplExpr e@Strings{}        = return ([], e)
simplExpr e@UnicodeStrings{} = return ([], e)
simplExpr e@Call{} = do
  (funStmts, funVar) <- simplVar $ call_fun e
  (argStmts, argVars) <- mapNameGen simplArgument $ call_args e
  return (funStmts ++ argStmts, e{call_fun = funVar, call_args = argVars})
-- Subscript
-- SlicedExpr
-- CondExpr
simplExpr e@BinaryOp{} = do
  (leftStmts, leftVar) <- simplVar $ left_op_arg e
  (rightStmts, rightVar) <- simplVar $ right_op_arg e
  return (leftStmts ++ rightStmts, e{ left_op_arg = leftVar
                                    , right_op_arg = rightVar
                                    } )
simplExpr e@UnaryOp{} = do (stmts, var) <- simplVar $ op_arg e
                           return (stmts, e{op_arg = var})
-- Lambda
simplExpr e@Tuple{} = do (stmts, vars) <- mapNameGen simplVar $ tuple_exprs e
                         return (stmts, e{tuple_exprs = vars})
-- Yield
-- Generator
-- ListComp
simplExpr e@List{} = do (stmts, vars) <- mapNameGen simplVar $ list_exprs e
                        return (stmts, e{list_exprs = vars})
-- Dictionary
-- DictComp
simplExpr e@Set{} = do (stmts, vars) <- mapNameGen simplVar $ set_exprs e
                       return (stmts, e{set_exprs = vars})
-- SetComp
-- Starred
simplExpr e@Paren{} = simplExpr $ paren_expr e
-- StringConversion
simplExpr e = error $
              "Simplifier.simplExpr called on unsupported expression: " ++
              render (pretty e)

-- Return ([simple statement])
simplStmt :: Statement a -> NameGen [Statement a]
--simplStmt s@Import{}          = 
--simplStmt s@FromImport{}      = 
simplStmt s@While{} = do
  (condStmts, condVar) <- simplVar $ while_cond s
  bodySuite <- simplSuite $ while_body s
  elseSuite <- simplSuite $ while_else s
  return $ condStmts ++ [ s{ while_cond = condVar
                           , while_body = bodySuite
                           , while_else = elseSuite
                           } ]
--simplStmt s@For{}             = 
simplStmt s@Fun{} = do
  (paramStmts, params) <- mapNameGen simplParameter $ fun_args s
  (annotStmts, annotExpr) <- simplExprMaybe $ fun_result_annotation s
  bodySuite <- simplSuite $ fun_body s
  return $ paramStmts ++ annotStmts ++ [ s{ fun_args = params
                                          , fun_result_annotation = annotExpr
                                          , fun_body = bodySuite
                                          } ]
--simplStmt s@Class{}           = 
--simplStmt s@Conditional{}     = 
--simplStmt s@Assign{}          = 
--simplStmt s@AugmentedAssign{} = 
--simplStmt s@Decorated{}       = 
--simplStmt s@Return{}          = 
--simplStmt s@Try{}             = 
--simplStmt s@Raise{}           = 
--simplStmt s@With{}            = 
--simplStmt s@Pass{}            = 
--simplStmt s@Break{}           = 
--simplStmt s@Continue{}        = 
--simplStmt s@Delete{}          = 
--simplStmt s@StmtExpr{}        = 
--simplStmt s@Global{}          = 
--simplStmt s@NonLocal{}        = 
--simplStmt s@Assert{}          = 
--simplStmt s@Print{}           = 
--simplStmt s@Exec{}            = 
simplStmt s = error $
              "Simplifier.simplStmt called on unsupported statement: " ++
              render (pretty s)

simplSuite :: Suite a -> NameGen (Suite a)
simplSuite = simplStmts

simplStmts :: [Statement a] -> NameGen [Statement a]
simplStmts ss = concat `liftM` mapM simplStmt ss

-- Return ([simple statement], simple parameter)
simplParameter :: Parameter a -> NameGen ([Statement a], Parameter a)
simplParameter p@Param{} = do
  (annotStmts, annotExpr) <- simplExprMaybe $ param_py_annotation p
  (defaultStmts, defaultExpr) <- simplExprMaybe $ param_default p
  return (annotStmts ++ defaultStmts, p{ param_py_annotation = annotExpr
                                       , param_default = defaultExpr})
simplParameter p = error $
                   "Simplifier.simplParameter called on unsupported parameter type: " ++
                   render (pretty p)

simplExprMaybe :: Maybe (Expr a) -> NameGen ([Statement a], Maybe (Expr a))
simplExprMaybe Nothing     = return ([], Nothing)
simplExprMaybe (Just expr) = do (exprStmts, expr') <- simplExpr expr
                                return (exprStmts, Just expr')
