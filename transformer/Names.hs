module Names where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.AST

namesExpr :: Expr a -> Set String
namesExpr e@Var{}        = namesIdent $ var_ident e
namesExpr e@Call{}       = Set.unions $ namesExpr (call_fun e) : map namesArgument (call_args e)
namesExpr e@Subscript{}  = Set.union (namesExpr $ subscriptee e) (namesExpr $ subscript_expr e)
namesExpr e@SlicedExpr{} = Set.unions $ namesExpr (slicee e) : map namesSlice (slices e)
namesExpr e@CondExpr{}   = Set.unions $ map namesExpr [ce_true_branch e, ce_condition e, ce_false_branch e]
namesExpr e@BinaryOp{}   = Set.union (namesExpr $ left_op_arg e) (namesExpr $ right_op_arg e)
namesExpr e@UnaryOp{}    = namesExpr $ op_arg e
namesExpr e@Lambda{}     = Set.unions $ namesExpr (lambda_body e) : map namesParameter (lambda_args e)
namesExpr e@Tuple{}      = Set.unions . map namesExpr $ tuple_exprs e
namesExpr e@Yield{}      = namesExprMaybe $ yield_expr e
namesExpr e@Generator{}  = namesComprehensionExpr $ gen_comprehension e
namesExpr e@ListComp{}   = namesComprehensionExpr $ list_comprehension e
namesExpr e@List{}       = Set.unions . map namesExpr $ list_exprs e
namesExpr e@Dictionary{} = Set.unions . map namesExprPair $ dict_mappings e
namesExpr e@DictComp{}   = namesComprehensionExprPair $ dict_comprehension e
namesExpr e@Set{}        = Set.unions . map namesExpr $ set_exprs e
namesExpr e@SetComp{}    = namesComprehensionExpr $ set_comprehension e
namesExpr e@Starred{}    = namesExpr $ starred_expr e
namesExpr e@Paren{}      = namesExpr $ paren_expr e
namesExpr e@StringConversion{} = namesExpr $ backquoted_expr e
namesExpr _ = Set.empty -- Constants

namesExprPair :: (Expr a, Expr b) -> Set String
namesExprPair (a, b) = Set.union (namesExpr a) (namesExpr b)

namesExprMaybe :: Maybe (Expr a) -> Set String
namesExprMaybe = maybe Set.empty namesExpr

namesIdent :: Ident a -> Set String
namesIdent = Set.singleton . ident_string

namesArgument :: Argument a -> Set String
namesArgument a@ArgKeyword{} = Set.insert (ident_string $ arg_keyword a) . namesExpr $ arg_expr a
namesArgument a              = namesExpr $ arg_expr a

namesParameter :: Parameter a -> Set String
namesParameter p@Param{} = Set.unions [ namesIdent $ param_name p
                                      , namesExprMaybe $ param_py_annotation p
                                      , namesExprMaybe $ param_default p
                                      ]
namesParameter p@VarArgsPos{} = Set.union (namesIdent $ param_name p)
                                          (namesExprMaybe $ param_py_annotation p)
namesParameter p@VarArgsKeyword{} = Set.union (namesIdent $ param_name p)
                                              (namesExprMaybe $ param_py_annotation p)
namesParameter EndPositional{} = Set.empty
namesParameter p@UnPackTuple{} = Set.union (namesParamTuple $ param_unpack_tuple p)
                                           (namesExprMaybe $ param_default p)

namesParamTuple :: ParamTuple a -> Set String
namesParamTuple p@ParamTupleName{} = namesIdent $ param_tuple_name p
namesParamTuple p@ParamTuple{}     = Set.unions . map namesParamTuple $ param_tuple p

namesSlice :: Slice a -> Set String
namesSlice s@SliceProper{} = Set.unions [ namesExprMaybe $ slice_lower s
                                        , namesExprMaybe $ slice_upper s
                                        , maybe Set.empty namesExprMaybe $ slice_stride s
                                        ]
namesSlice s@SliceExpr{} = namesExpr $ slice_expr s
namesSlice SliceEllipsis{} = Set.empty

namesComprehensionExpr :: Comprehension (Expr a) b -> Set String
namesComprehensionExpr c = Set.union (namesExpr $ comprehension_expr c)
                                     (namesCompFor $ comprehension_for c)

namesComprehensionExprPair :: Comprehension (Expr a, Expr b) c -> Set String
namesComprehensionExprPair c = Set.union exprPairNames compForNames
  where exprPairNames = namesExprPair $ comprehension_expr c
        compForNames = namesCompFor $ comprehension_for c

namesCompFor :: CompFor a -> Set String
namesCompFor c = Set.unions $ Set.union inExpr forIter : forExprs
  where inExpr   = namesExpr $ comp_in_expr c
        forIter  = namesCompIterMaybe $ comp_for_iter c
        forExprs = map namesExpr $ comp_for_exprs c

namesCompIter :: CompIter a -> Set String
namesCompIter c@IterFor{} = namesCompFor $ comp_iter_for c
namesCompIter c@IterIf{}  = namesCompIf $ comp_iter_if c

namesCompIterMaybe :: Maybe (CompIter a) -> Set String
namesCompIterMaybe = maybe Set.empty namesCompIter

namesCompIf :: CompIf a -> Set String
namesCompIf c = Set.union (namesExpr $ comp_if c) (namesCompIterMaybe $ comp_if_iter c)
