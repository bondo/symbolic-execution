module SymbolicAnnotation (symbStmt) where

import Control.Monad (msum)

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()

mkIdent :: String -> Ident a
mkIdent = error "SymbolicAnnotation.mkIdent not implemented"

mkVar :: Ident a -> Expr a
mkVar i = Var i $ ident_annot i

mkCall :: Ident a -> [Expr a] -> Statement a
mkCall = error "SymbolicAnnotation.mkCall not implemented"

mkIntroScope :: [Ident a] -> Statement a
mkIntroScope = error "SymbolicAnnotation.mkIntroScope not implemented"

endScope :: Statement a
endScope = mkCall (mkIdent "symbolic_return") []

symbStmt :: Statement a -> [Statement a]
--symbStmt s@Import{}          = 
--symbStmt s@FromImport{}      = 
--symbStmt s@While{}           = 
--symbStmt s@For{}             = 
--symbStmt s@Fun{}             = 
--symbStmt s@Class{}           = 
--symbStmt s@Conditional{}     = 
symbStmt s@Assign{} = case msum [litAss, callAss, opAss] of
  Just stmt -> [stmt, s]
  Nothing   -> badStatement s
  where lhs = if length (assign_to s) /= 1
              then badStatement s
              else getIdent s . head $ assign_to s
        rhs = assign_expr s
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

litAssIdent :: Ident a
litAssIdent = mkIdent "symbolic_assign_literal"

getLiteralAssign :: Ident a -> Expr a -> Maybe (Statement a)
getLiteralAssign i e@Int{}            = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@LongInt{}        = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@Float{}          = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@Imaginary{}      = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@Bool{}           = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@None{}           = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@ByteStrings{}    = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@Strings{}        = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign i e@UnicodeStrings{} = Just $ mkCall litAssIdent [mkVar i, e]
getLiteralAssign _ _                  = Nothing

getCallAssignment :: Ident a -> Expr a -> Maybe (Statement a)
getCallAssignment = error "SymbolicAnnotation.getCallAssignment not implemented"

getOpAssignment :: Ident a -> Expr a -> Maybe (Statement a)
getOpAssignment = error "SymbolicAnnotation.getOpAssignment not implemented"

getIdent :: Statement a -> Expr a -> Ident a
getIdent _ e@Var{} = var_ident e
getIdent s _       = badStatement s

badStatement s = error $
                 "SymbolicAnnotation.symbStmt called on unsupported statement: " ++
                 render (pretty s)
