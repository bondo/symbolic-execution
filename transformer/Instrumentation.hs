{-# LANGUAGE PackageImports #-}

module Instrumentation (instStmt, instModule) where

import Control.Monad (msum, liftM)
import "mtl" Control.Monad.Error ()
import Data.List (transpose)
import Data.Maybe (maybeToList)

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.SrcLocation ( SrcSpan(SpanEmpty) )

mkIdent :: String -> IdentSpan
mkIdent s = Ident { ident_string = s, ident_annot = SpanEmpty }

mkVar :: IdentSpan -> ExprSpan
mkVar i = Var { var_ident = i, expr_annot = SpanEmpty }

mkString :: IdentSpan -> ExprSpan
mkString i = Strings { strings_strings = ["'" ++ ident_string i ++ "'"], expr_annot = SpanEmpty }

mkArg :: ExprSpan -> ArgumentSpan
mkArg e = ArgExpr { arg_expr = e, arg_annot = SpanEmpty }

mkCall :: IdentSpan -> [ExprSpan] -> StatementSpan
mkCall i es = StmtExpr { stmt_expr = expr, stmt_annot = SpanEmpty }
  where expr = Call { call_fun = mkVar i, call_args = map mkArg es, expr_annot = SpanEmpty }

mkIntroScope :: [IdentSpan] -> StatementSpan
mkIntroScope = mkCall (mkIdent "symbolic_scope") . map mkString

mkReturn :: Maybe IdentSpan -> StatementSpan
mkReturn = mkCall (mkIdent "symbolic_return") . map mkString . maybeToList

mkAssert :: IdentSpan -> StatementSpan
mkAssert var = mkCall (mkIdent "symbolic_assert") [mkString var]

mkRefute :: IdentSpan -> StatementSpan
mkRefute var = mkCall (mkIdent "symbolic_refute") [mkString var]

instModule :: ModuleSpan -> Either String ModuleSpan
instModule (Module m) = Module `liftM` instSuite m

instSuite :: SuiteSpan -> Either String SuiteSpan
instSuite s = concat `liftM` mapM instStmt s

instStmt :: StatementSpan -> Either String [StatementSpan]
--instStmt s@Import{}          = 
--instStmt s@FromImport{}      = 
instStmt s@While{while_cond = Var{var_ident = cond}, while_body = body, while_else = []} =
  Right [s{ while_body = mkAssert cond : body }]
--instStmt s@For{}             = 
instStmt s@Fun{fun_args = params, fun_result_annotation = Nothing, fun_body = body}
  | Just idents <- mapM getIdent params = buildStmt idents `liftM` instSuite body
  where getIdent Param{ param_name = i
                      , param_py_annotation = Nothing
                      , param_default = Nothing} = Just i
        getIdent _  = Nothing
        buildStmt idents b = [s{fun_body = mkIntroScope idents : b}]
--instStmt s@Class{}           = 
instStmt s@Conditional{cond_guards = [(cond@Var{var_ident = i}, thenSuite)], cond_else = elseSuile} =
  do thenSuite' <- instSuite thenSuite
     elseSuile' <- instSuite elseSuile
     Right [s { cond_guards = [(cond, mkAssert i : thenSuite')]
              , cond_else = mkRefute i : elseSuile'
              }]
instStmt s@Assign{assign_to = [Var{var_ident = lhs}], assign_expr = rhs}
  | Just stmt <- msum [litAss, callAss, opAss] = Right [stmt, s]
  where litAss  = getLiteralAssign lhs rhs
        callAss = getCallAssignment lhs rhs
        opAss   = getOpAssignment lhs rhs
--instStmt s@AugmentedAssign{} = 
--instStmt s@Decorated{}       = 
instStmt s@Return{return_expr = Just Var{var_ident = i}} = Right [mkReturn $ Just i, s]
instStmt s@Return{return_expr = Nothing}                 = Right [mkReturn $ Nothing, s]
--instStmt s@Try{}             = 
--instStmt s@Raise{}           = 
--instStmt s@With{}            = 
instStmt s@Pass{}     = Right [s]
instStmt s@Break{}    = Right [s]
instStmt s@Continue{} = Right [s]
--instStmt s@Delete{}          = 
instStmt s@StmtExpr{stmt_expr = e} | Just (fun, args) <- getFunction e =
  Right [mkCall (mkIdent "symbolic_call") $ fun : args, s]
--instStmt s@Global{}          = 
--instStmt s@NonLocal{}        = 
--instStmt s@Assert{}          = 
--instStmt s@Print{}           = 
--instStmt s@Exec{}            = 
instStmt s = Left $ "Instrumentation.instStmt called on unsupported statement:\n" ++
             render (pretty s)

litAssIdent :: IdentSpan
litAssIdent = mkIdent "symbolic_assign_literal"

callAssIdent :: IdentSpan
callAssIdent = mkIdent "symbolic_assign_call"

opAssIdent :: IdentSpan
opAssIdent = mkIdent "symbolic_assign_binop"

getLiteralAssign :: IdentSpan -> ExprSpan -> Maybe StatementSpan
getLiteralAssign i e@Int{}            = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@LongInt{}        = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@Float{}          = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@Imaginary{}      = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@Bool{}           = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@None{}           = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@ByteStrings{}    = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@Strings{}        = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign i e@UnicodeStrings{} = Just $ mkCall litAssIdent [mkString i, e]
getLiteralAssign _ _                  = Nothing

getFunction ::
  ExprSpan -- Normalized function call
  -> Maybe (   -- Given f(a,b,c):
    ExprSpan,  --   "f"
    [ExprSpan] --   ["a",a,"b",b,"c",c]
    )
getFunction Call{call_fun = Var{var_ident = fun}, call_args = args}
  | Just strs <- mapM (argTo mkString) args
  , Just vars <- mapM (argTo mkVar) args =
  Just (mkString fun, concat $ transpose [strs, vars])
  where argTo to ArgExpr{arg_expr = Var{var_ident = ident}} = Just $ to ident
        argTo _ _ = Nothing
getFunction _ = Nothing

getCallAssignment :: IdentSpan -> ExprSpan -> Maybe StatementSpan
getCallAssignment i e | Just (fun, args) <- getFunction e =
  Just . mkCall callAssIdent $ [mkString i, fun] ++ args
getCallAssignment _ _ = Nothing

getOpAssignment :: IdentSpan -> ExprSpan -> Maybe StatementSpan
getOpAssignment i b@BinaryOp{ left_op_arg = Var{var_ident = left}
                            , right_op_arg = Var{var_ident = right} } =
  Just $ mkCall opAssIdent [mkString i, op, mkString left, mkString right]
  where op = mkString . mkIdent . render . pretty $ operator b
getOpAssignment _ _ = Nothing
