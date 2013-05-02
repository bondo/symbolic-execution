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
simplArgument a@ArgExpr{} = do (exprStmts, exprVar) <- simplVar $ arg_expr a
                               return (exprStmts, a{arg_expr = exprVar})
simplArgument a = error $
                  "Simplifier.simplArgument called on unsupported argument: " ++
                  render (pretty a)

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
simplStmt s@Import{} = return [s]
simplStmt s@FromImport{} = return [s]
simplStmt s@While{} = do
  (condStmts, condVar) <- simplVar $ while_cond s
  bodySuite <- simplSuite $ while_body s
  elseSuite <- simplSuite $ while_else s
  return $ condStmts ++ [ s{ while_cond = condVar
                           , while_body = bodySuite
                           , while_else = elseSuite
                           } ]
simplStmt s@For{} = do
  v <- getTarget $ for_targets s
  (generatorStmts, generatorVar) <- simplVar $ for_generator s
  bodySuite <- simplSuite $ for_body s
  elseSuite <- simplSuite $ for_else s
  return $ generatorStmts  ++ [ s{ for_generator = generatorVar
                                 , for_body = bodySuite
                                 , for_else = elseSuite
                                 } ]
  where getTarget [v@Var{}] = v
        getTarget _ = error $ "Unsupported target list in for-loop: "
                      ++ render (pretty e)
simplStmt s@Fun{} = do
  (paramStmts, params) <- mapNameGen simplParameter $ fun_args s
  (annotStmts, annotExpr) <- simplExprMaybe $ fun_result_annotation s
  bodySuite <- simplSuite $ fun_body s
  return $ paramStmts ++ annotStmts ++ [ s{ fun_args = params
                                          , fun_result_annotation = annotExpr
                                          , fun_body = bodySuite
                                          } ]
simplStmt s@Class{} = do
  (paramStmts, params) <- mapNameGen simplArgument $ class_args s
  bodySuite <- simplSuite $ class_body s
  return $ paramStmts ++ [ s{ class_args = params
                            , class_body = bodySuite
                            } ]
simplStmt s@Conditional{} =
  if null guards
  then error "Simplifier.simplStmt called on conditional with no guards"
  else if null gs
       -- Only one guard
       then do (guardStmts, guardVar) <- simplVar guardExpr
               guardSuite' <- simplSuite guardSuite
               elseSuite <- simplSuite $ cond_else s
               return $ guardStmts ++ [ s{ cond_guards = [(guardVar, guardSuite')]
                                         , cond_else = elseSuite
                                         } ]
       -- Many guards
       else simplStmt s{cond_guards = [g], cond_else = [innerCond]}
  where guards = cond_guards s
        g@(guardExpr, guardSuite) : gs = guards
        innerCond = s{cond_guards = gs}
simplStmt s@Assign{} = if length (assign_to s) /= 1
                       then error "Simplifier.simplStmt: Only single LHS assignment supported."
                       else do (lhsStmts, lhsVar) <- simplVar lhs
                               (rhsStmts, rhsExpr) <- simplExpr $ assign_expr s
                               return $ lhsStmts ++ rhsStmts ++ [ s{ assign_to = [lhsVar]
                                                                   , assign_expr = rhsExpr
                                                                   } ]
  where [lhs] = assign_to s
simplStmt s@AugmentedAssign{} = do
  (lhsStmts, lhsVar) <- simplVar $ aug_assign_to s
  (rhsStmts, rhsVar) <- simplVar $ aug_assign_expr s
  return $ lhsStmts ++ rhsStmts ++ [ Assign [lhsVar] (op lhsVar rhsVar) $ stmt_annot s ]
  where op = assignOpToBinOp $ aug_assign_op s
--simplStmt s@Decorated{}       = 
simplStmt s@Return{} = do (valStmts, valExpr) <- simplExprMaybe $ return_expr s
                          return $ valStmts ++ [ s{return_expr = valExpr} ]
--simplStmt s@Try{}             = 
--simplStmt s@Raise{}           = 
--simplStmt s@With{}            = 
simplStmt s@Pass{}     = return [s]
simplStmt s@Break{}    = return [s]
simplStmt s@Continue{} = return [s]
--simplStmt s@Delete{}          = 
simplStmt s@StmtExpr{} = do (exprStmts, expr) <- simplExpr $ stmt_expr s
                            return $ exprStmts ++ [ s{stmt_expr = expr} ]
simplStmt s@Global{}   = return [s]
simplStmt s@NonLocal{} = return [s]
simplStmt s@Assert{}   = if length exprs /= 1
                         then error $ "Simplifier.simplStmt - Assert form not supported: " ++
                              render (pretty s)
                         else do (exprStmts, var) <- simplVar $ head exprs
                                 return $ exprStmts ++ [ s{assert_exprs = [var]} ]
  where exprs = assert_exprs s
simplStmt s@Print{} = do (exprStmts, exprs) <- mapNameGen simplExpr $ print_exprs s
                         return $ exprStmts ++ [ s{print_exprs = exprs} ]
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
                   "Simplifier.simplParameter called on unsupported parameter: " ++
                   render (pretty p)

simplExprMaybe :: Maybe (Expr a) -> NameGen ([Statement a], Maybe (Expr a))
simplExprMaybe Nothing     = return ([], Nothing)
simplExprMaybe (Just expr) = do (exprStmts, expr') <- simplExpr expr
                                return (exprStmts, Just expr')

makeBinOp :: Op a -> a -> Expr a -> Expr a -> Expr a
makeBinOp op a l r = BinaryOp op l r a

assignOpToBinOp :: AssignOp a -> Expr a -> Expr a -> Expr a
assignOpToBinOp (PlusAssign a) = makeBinOp (Plus a) a
assignOpToBinOp (MinusAssign a) = makeBinOp (Minus a) a
assignOpToBinOp (MultAssign a) = makeBinOp (Multiply a) a
assignOpToBinOp (DivAssign a) = makeBinOp (Divide a) a
assignOpToBinOp (ModAssign a) = makeBinOp (Modulo a) a
assignOpToBinOp (PowAssign a) = makeBinOp (Exponent a) a
assignOpToBinOp (BinAndAssign a) = makeBinOp (BinaryAnd a) a
assignOpToBinOp (BinOrAssign a) = makeBinOp (BinaryOr a) a
assignOpToBinOp (BinXorAssign a) = makeBinOp (Xor a) a
assignOpToBinOp (LeftShiftAssign a) = makeBinOp (ShiftLeft a) a
assignOpToBinOp (RightShiftAssign a) = makeBinOp (ShiftRight a) a
assignOpToBinOp (FloorDivAssign a) = makeBinOp (FloorDivide a) a

simplModule :: Module a -> NameGen (Module a)
simplModule (Module ss) = Module `liftM` simplStmts ss
