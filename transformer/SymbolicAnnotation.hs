module SymbolicAnnotation (symbStmt, symbModule) where

import Control.Monad (msum)
import Data.List (transpose)

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
mkIntroScope = error "SymbolicAnnotation.mkIntroScope not implemented"

endScope :: StatementSpan
endScope = mkCall (mkIdent "symbolic_return") []

mkAssert :: IdentSpan -> StatementSpan
mkAssert var = mkCall (mkIdent "symbolic_assert") [mkString var]

mkRefute :: IdentSpan -> StatementSpan
mkRefute var = mkCall (mkIdent "symbolic_refute") [mkString var]

symbModule :: ModuleSpan -> ModuleSpan
symbModule (Module m) = Module $ concatMap symbStmt m

symbStmt :: StatementSpan -> [StatementSpan]
--symbStmt s@Import{}          = 
--symbStmt s@FromImport{}      = 
--symbStmt s@While{}           = 
--symbStmt s@For{}             = 
--symbStmt s@Fun{}             = 
--symbStmt s@Class{}           = 
symbStmt s@Conditional{} | [(sCond@Var{var_ident = i}, sSuite)] <- cond_guards s =
  [s { cond_guards = [(sCond, mkAssert i : sSuite)]
     , cond_else = mkRefute i : cond_else s
     }]
symbStmt s@Assign{assign_to = [target], assign_expr = rhs}
  | Just stmt <- msum [litAss, callAss, opAss] = [stmt, s]
  where lhs = getIdent s target
        litAss  = getLiteralAssign lhs rhs
        callAss = getCallAssignment lhs rhs
        opAss   = getOpAssignment lhs rhs
--symbStmt s@AugmentedAssign{} = 
--symbStmt s@Decorated{}       = 
symbStmt s@Return{} = [endScope, s]
--symbStmt s@Try{}             = 
--symbStmt s@Raise{}           = 
--symbStmt s@With{}            = 
symbStmt s@Pass{}     = [s]
symbStmt s@Break{}    = [s]
symbStmt s@Continue{} = [s]
--symbStmt s@Delete{}          = 
--symbStmt s@StmtExpr{}        = 
--symbStmt s@Global{}          = 
--symbStmt s@NonLocal{}        = 
--symbStmt s@Assert{}          = 
--symbStmt s@Print{}           = 
--symbStmt s@Exec{}            = 
symbStmt s = badStatement s

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

getCallAssignment :: IdentSpan -> ExprSpan -> Maybe StatementSpan
getCallAssignment i Call{call_fun = Var{var_ident = fun}, call_args = args}
  | Just strs <- mapM (argTo mkString) args
  , Just vars <- mapM (argTo mkVar) args =
  Just . mkCall callAssIdent $ [mkString i, mkString fun] ++ concat (transpose [strs, vars])
  where argTo to ArgExpr{arg_expr = Var{var_ident = i}} = Just $ to i
        argTo _ _ = Nothing
getCallAssignment _ _ = Nothing

getOpAssignment :: IdentSpan -> ExprSpan -> Maybe StatementSpan
getOpAssignment = error "SymbolicAnnotation.getOpAssignment not implemented"

getIdent :: StatementSpan -> ExprSpan -> IdentSpan
getIdent _ Var{var_ident = i} = i
getIdent s _                  = badStatement s

badStatement s = error $
                 "SymbolicAnnotation.symbStmt called on unsupported statement: " ++
                 render (pretty s)
