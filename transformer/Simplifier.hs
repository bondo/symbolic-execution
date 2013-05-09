{-# LANGUAGE PackageImports #-}

module Simplifier where

import "mtl" Control.Monad.State
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.SrcLocation ( SrcSpan(SpanEmpty) )

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
sequenceNameGen (n:ns) = do (as,  b)  <- n
                            (as', bs) <- sequenceNameGen ns
                            return (as ++ as', b : bs)

mapNameGen :: (a -> NameGen ([b], c)) -> [a] -> NameGen ([b], [c])
mapNameGen f = sequenceNameGen . map f

mkVar :: IdentSpan -> ExprSpan
mkVar i = Var i SpanEmpty

newVar :: NameGen ExprSpan
newVar = do name <- freshName
            return . mkVar $ Ident name SpanEmpty

mkAssign :: ExprSpan -> ExprSpan -> StatementSpan
mkAssign lhs rhs = Assign [lhs] rhs SpanEmpty

-- Return ([simple statement], Var expression)
simplVar :: ExprSpan -> NameGen ([StatementSpan], ExprSpan)
simplVar e@Var{} = return ([], e)
simplVar e = do (stmts, expr) <- simplExpr e
                var           <- newVar
                return (stmts ++ [mkAssign var expr], var)

-- Return ([simple statement], simple argument)
simplArgument :: ArgumentSpan -> NameGen ([StatementSpan], ArgumentSpan)
simplArgument a@ArgExpr{arg_expr = expr} = do
  (exprStmts, exprVar) <- simplVar expr
  return (exprStmts, a{arg_expr = exprVar})
simplArgument a = error $
                  "Simplifier.simplArgument called on unsupported argument:\n" ++
                  render (pretty a)

-- Return ([simple statement], simple expression)
simplExpr :: ExprSpan -> NameGen ([StatementSpan], ExprSpan)
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
simplExpr e@Call{call_fun = fun, call_args = args} = do
  (funStmts, funVar)  <- simplVar fun
  (argStmts, argVars) <- mapNameGen simplArgument args
  return (funStmts ++ argStmts, e{call_fun = funVar, call_args = argVars})
-- Subscript
-- SlicedExpr
-- CondExpr
simplExpr e@BinaryOp{left_op_arg = left, right_op_arg = right} = do
  (leftStmts, leftVar)   <- simplVar left
  (rightStmts, rightVar) <- simplVar right
  return (leftStmts ++ rightStmts, e{ left_op_arg  = leftVar
                                    , right_op_arg = rightVar
                                    } )
simplExpr e@UnaryOp{op_arg = arg} = do
  (stmts, var) <- simplVar arg
  return (stmts, e{op_arg = var})
-- Lambda
simplExpr e@Tuple{tuple_exprs = exprs} = do
  (stmts, vars) <- mapNameGen simplVar exprs
  return (stmts, e{tuple_exprs = vars})
-- Yield
-- Generator
-- ListComp
simplExpr e@List{list_exprs = exprs} = do
  (stmts, vars) <- mapNameGen simplVar exprs
  return (stmts, e{list_exprs = vars})
-- Dictionary
-- DictComp
simplExpr e@Set{set_exprs = exprs} = do
  (stmts, vars) <- mapNameGen simplVar exprs
  return (stmts, e{set_exprs = vars})
-- SetComp
-- Starred
simplExpr e@Paren{paren_expr = expr} = simplExpr expr
-- StringConversion
simplExpr e = error $
              "Simplifier.simplExpr called on unsupported expression:\n" ++
              render (pretty e)

-- Return ([simple statement])
simplStmt :: StatementSpan -> NameGen [StatementSpan]
simplStmt s@Import{}     = return [s]
simplStmt s@FromImport{} = return [s]
simplStmt s@While{while_cond = Var{}, while_body = wBody, while_else = wElse} = do
  bodySuite            <- simplSuite wBody
  elseSuite            <- simplSuite wElse
  return [s{while_body = bodySuite, while_else = elseSuite}]
simplStmt s@For{for_generator = fGen, for_body = fBody, for_else = fElse, for_targets = [Var{}]} = do
  (genStmts, genVar) <- simplVar fGen
  bodySuite          <- simplSuite fBody
  elseSuite          <- simplSuite fElse
  return $ genStmts ++ [ s{ for_generator = genVar
                          , for_body      = bodySuite
                          , for_else      = elseSuite
                          } ]
simplStmt s@Fun{fun_args = args, fun_result_annotation = res, fun_body = body} = do
  (paramStmts, params)    <- mapNameGen simplParameter args
  (annotStmts, annotExpr) <- simplExprMaybe res
  bodySuite               <- simplSuite body
  return $ paramStmts ++ annotStmts ++ [ s{ fun_args              = params
                                          , fun_result_annotation = annotExpr
                                          , fun_body              = bodySuite
                                          } ]
simplStmt s@Class{class_args = args, class_body = body} = do
  (paramStmts, params) <- mapNameGen simplArgument args
  bodySuite            <- simplSuite body
  return $ paramStmts ++ [ s{ class_args = params
                            , class_body = bodySuite
                            } ]
simplStmt s@Conditional{cond_guards = [(guardExpr, guardSuite)], cond_else = cElse} = do
  (guardStmts, guardVar) <- simplVar guardExpr
  guardSuite'            <- simplSuite guardSuite
  elseSuite              <- simplSuite cElse
  return $ guardStmts ++ [ s{ cond_guards = [(guardVar, guardSuite')]
                            , cond_else   = elseSuite
                            } ]
simplStmt s@Conditional{cond_guards = g:gs} =
  simplStmt s{cond_guards = [g], cond_else = [s{cond_guards = gs}]}
simplStmt s@Assign{assign_to = [lhs], assign_expr = expr} = do
  (lhsStmts, lhsVar)  <- simplVar lhs
  (rhsStmts, rhsExpr) <- simplExpr expr
  return $ lhsStmts ++ rhsStmts ++ [ s{ assign_to   = [lhsVar]
                                      , assign_expr = rhsExpr
                                      } ]
simplStmt AugmentedAssign{aug_assign_to = lhs, aug_assign_expr = expr, aug_assign_op = op} = do
  (lhsStmts, lhsVar) <- simplVar lhs
  (rhsStmts, rhsVar) <- simplVar expr
  return $ lhsStmts ++ rhsStmts ++ [ Assign [lhsVar] (binOp lhsVar rhsVar) SpanEmpty ]
  where binOp = assignOpToBinOp op
--simplStmt s@Decorated{}       = 
simplStmt s@Return{return_expr = Just expr} = do
  (valStmts, valExpr) <- simplVar expr
  return $ valStmts ++ [ s{return_expr = Just valExpr} ]
simplStmt s@Return{return_expr = Nothing} = return [s]
--simplStmt s@Try{}             = 
--simplStmt s@Raise{}           = 
--simplStmt s@With{}            = 
simplStmt s@Pass{}     = return [s]
simplStmt s@Break{}    = return [s]
simplStmt s@Continue{} = return [s]
--simplStmt s@Delete{}          = 
simplStmt s@StmtExpr{stmt_expr = expr} = do
  (exprStmts, expr') <- simplExpr expr
  return $ exprStmts ++ [ s{stmt_expr = expr'} ]
simplStmt s@Global{}   = return [s]
simplStmt s@NonLocal{} = return [s]
simplStmt s@Assert{assert_exprs = [expr]} = do
  (exprStmts, var) <- simplVar expr
  return $ exprStmts ++ [ s{assert_exprs = [var]} ]
simplStmt s@Print{print_exprs = exprs} = do
  (exprStmts, exprs) <- mapNameGen simplExpr exprs
  return $ exprStmts ++ [ s{print_exprs = exprs} ]
--simplStmt s@Exec{}            =
simplStmt s = error $
              "Simplifier.simplStmt called on unsupported statement:\n" ++
              render (pretty s)

simplSuite :: SuiteSpan -> NameGen SuiteSpan
simplSuite = simplStmts

simplStmts :: [StatementSpan] -> NameGen [StatementSpan]
simplStmts ss = concat `liftM` mapM simplStmt ss

-- Return ([simple statement], simple parameter)
simplParameter :: ParameterSpan -> NameGen ([StatementSpan], ParameterSpan)
simplParameter p@Param{param_py_annotation = pAnnot, param_default = pDefault} = do
  (annotStmts, annotExpr)     <- simplExprMaybe pAnnot
  (defaultStmts, defaultExpr) <- simplExprMaybe pDefault
  return (annotStmts ++ defaultStmts, p{ param_py_annotation = annotExpr
                                       , param_default       = defaultExpr})
simplParameter p = error $
                   "Simplifier.simplParameter called on unsupported parameter:\n" ++
                   render (pretty p)

simplExprMaybe :: Maybe ExprSpan -> NameGen ([StatementSpan], Maybe ExprSpan)
simplExprMaybe Nothing     = return ([], Nothing)
simplExprMaybe (Just expr) = do (exprStmts, expr') <- simplExpr expr
                                return (exprStmts, Just expr')

makeBinOp :: (SrcSpan -> OpSpan) -> ExprSpan -> ExprSpan -> ExprSpan
makeBinOp op l r = BinaryOp (op SpanEmpty) l r SpanEmpty

assignOpToBinOp :: AssignOpSpan -> ExprSpan -> ExprSpan -> ExprSpan
assignOpToBinOp PlusAssign{}       = makeBinOp Plus
assignOpToBinOp MinusAssign{}      = makeBinOp Minus
assignOpToBinOp MultAssign{}       = makeBinOp Multiply
assignOpToBinOp DivAssign{}        = makeBinOp Divide
assignOpToBinOp ModAssign{}        = makeBinOp Modulo
assignOpToBinOp PowAssign{}        = makeBinOp Exponent
assignOpToBinOp BinAndAssign{}     = makeBinOp BinaryAnd
assignOpToBinOp BinOrAssign{}      = makeBinOp BinaryOr
assignOpToBinOp BinXorAssign{}     = makeBinOp Xor
assignOpToBinOp LeftShiftAssign{}  = makeBinOp ShiftLeft
assignOpToBinOp RightShiftAssign{} = makeBinOp ShiftRight
assignOpToBinOp FloorDivAssign{}   = makeBinOp FloorDivide

simplModule :: ModuleSpan -> NameGen ModuleSpan
simplModule (Module ss) = Module `liftM` simplStmts ss
