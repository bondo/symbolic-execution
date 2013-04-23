module Names (namesExpr, namesStmt, namesStmts, namesModule) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.Python.Common.AST

namesModule :: Module a -> Set String
namesModule (Module stmts) = namesStmts stmts

namesStmts :: [Statement a] -> Set String
namesStmts = Set.unions . map namesStmt

namesStmt :: Statement a -> Set String
namesStmt s@Import{}     = Set.unions . map namesImportItem $ import_items s
namesStmt s@FromImport{} = Set.union (namesImportRelative $ from_module s) (namesFromItems $ from_items s)
namesStmt s@While{}      = Set.unions [ namesExpr $ while_cond s
                                      , namesSuite $ while_body s
                                      , namesSuite $ while_else s
                                      ]
namesStmt s@For{} = Set.unions [ namesExpr $ for_generator s
                               , namesSuite $ for_body s
                               , namesSuite $ for_else s
                               , namesExprs $ for_targets s
                               ]
namesStmt s@Fun{} = Set.unions [ namesIdent $ fun_name s
                               , namesParameters $ fun_args s
                               , namesExprMaybe $ fun_result_annotation s
                               , namesSuite $ fun_body s
                               ]
namesStmt s@Class{} = Set.unions [ namesIdent $ class_name s
                                 , namesArguments $ class_args s
                                 , namesSuite $ class_body s
                                 ]
namesStmt s@Conditional{} = Set.unions $ (namesSuite $ cond_else s) : guardNames
  where guardNames = map (\(e,su) -> Set.union (namesExpr e) (namesSuite su)) $ cond_guards s
namesStmt s@Assign{}          = Set.union (namesExprs $ assign_to s) (namesExpr $ assign_expr s)
namesStmt s@AugmentedAssign{} = Set.union (namesExpr $ aug_assign_to s)
                                          (namesExpr $ aug_assign_expr s)
namesStmt s@Decorated{} = Set.unions $ (namesStmt $ decorated_def s) : decoratorsNames
  where decoratorsNames = map namesDecorator $ decorated_decorators s
namesStmt s@Return{} = namesExprMaybe $ return_expr s
namesStmt s@Try{}    = Set.unions [ namesSuite $ try_body s
                                  , Set.unions . map namesHandler $ try_excepts s
                                  , namesSuite $ try_else s
                                  , namesSuite $ try_finally s
                                  ]
namesStmt s@Raise{} = namesRaiseExpr $ raise_expr s
namesStmt s@With{}  = Set.unions $ (namesSuite $ with_body s) : contextNames
  where contextNames = map namesExprAndMaybeExpr $ with_context s
namesStmt s@Delete{}   = namesExprs $ del_exprs s
namesStmt s@StmtExpr{} = namesExpr $ stmt_expr s
namesStmt s@Global{}   = namesIdents $ global_vars s
namesStmt s@NonLocal{} = namesIdents $ nonLocal_vars s
namesStmt s@Assert{}   = namesExprs $ assert_exprs s
namesStmt s@Print{}    = namesExprs $ print_exprs s
namesStmt s@Exec{}     = Set.union (namesExpr $ exec_expr s) globalsLocalsNames
  where globalsLocalsNames = maybe Set.empty namesExprAndMaybeExpr $ exec_globals_locals s
namesStmt _ = Set.empty -- Pass, Break, Continue

namesImportItem :: ImportItem a -> Set String
namesImportItem i = Set.union (namesDottedName $ import_item_name i)
                              (namesIdentMaybe $ import_as_name i)

namesImportRelative :: ImportRelative a -> Set String
namesImportRelative = maybe Set.empty namesDottedName . import_relative_module

namesFromItems :: FromItems a -> Set String
namesFromItems ImportEverything{} = Set.empty
namesFromItems f@FromItems{}      = Set.unions . map namesFromItem $ from_items_items f

namesFromItem :: FromItem a -> Set String
namesFromItem f = Set.union (namesIdent $ from_item_name f) (namesIdentMaybe $ from_as_name f)

namesSuite :: Suite a -> Set String
namesSuite = namesStmts

namesDecorator :: Decorator a -> Set String
namesDecorator d = Set.union (namesDottedName $ decorator_name d) (namesArguments $ decorator_args d)

namesHandler :: Handler a -> Set String
namesHandler h = Set.union (namesExceptClause $ handler_clause h) (namesSuite $ handler_suite h)

namesRaiseExpr :: RaiseExpr a -> Set String
namesRaiseExpr (RaiseV3 r) = maybe Set.empty namesExprAndMaybeExpr r
namesRaiseExpr (RaiseV2 r) = maybe Set.empty work r
  where work (e, meme) = Set.union (namesExpr e) (maybe Set.empty namesExprAndMaybeExpr meme)

namesDottedName :: DottedName a -> Set String
namesDottedName = namesIdents

namesExceptClause :: ExceptClause a -> Set String
namesExceptClause e = maybe Set.empty namesExprAndMaybeExpr $ except_clause e

namesExprAndMaybeExpr :: (Expr a, Maybe (Expr b)) -> Set String
namesExprAndMaybeExpr (e,em) = Set.union (namesExpr e) (namesExprMaybe em)

namesExprs :: [Expr a] -> Set String
namesExprs = Set.unions . map namesExpr

namesExpr :: Expr a -> Set String
namesExpr e@Var{}        = namesIdent $ var_ident e
namesExpr e@Call{}       = Set.union (namesExpr $ call_fun e) (namesArguments $ call_args e)
namesExpr e@Subscript{}  = Set.union (namesExpr $ subscriptee e) (namesExpr $ subscript_expr e)
namesExpr e@SlicedExpr{} = Set.unions $ namesExpr (slicee e) : map namesSlice (slices e)
namesExpr e@CondExpr{}   = namesExprs [ce_true_branch e, ce_condition e, ce_false_branch e]
namesExpr e@BinaryOp{}   = Set.union (namesExpr $ left_op_arg e) (namesExpr $ right_op_arg e)
namesExpr e@UnaryOp{}    = namesExpr $ op_arg e
namesExpr e@Lambda{}     = Set.union (namesExpr $ lambda_body e) (namesParameters $ lambda_args e)
namesExpr e@Tuple{}      = namesExprs $ tuple_exprs e
namesExpr e@Yield{}      = namesExprMaybe $ yield_expr e
namesExpr e@Generator{}  = namesComprehensionExpr $ gen_comprehension e
namesExpr e@ListComp{}   = namesComprehensionExpr $ list_comprehension e
namesExpr e@List{}       = namesExprs $ list_exprs e
namesExpr e@Dictionary{} = Set.unions . map namesExprPair $ dict_mappings e
namesExpr e@DictComp{}   = namesComprehensionExprPair $ dict_comprehension e
namesExpr e@Set{}        = namesExprs $ set_exprs e
namesExpr e@SetComp{}    = namesComprehensionExpr $ set_comprehension e
namesExpr e@Starred{}    = namesExpr $ starred_expr e
namesExpr e@Paren{}      = namesExpr $ paren_expr e
namesExpr e@StringConversion{} = namesExpr $ backquoted_expr e
-- Int, LongInt, Float, Imaginary, Bool, None, Ellipsis, ByteStrings, Strings, UnicodeStrings
namesExpr _ = Set.empty

namesExprPair :: (Expr a, Expr b) -> Set String
namesExprPair (a, b) = Set.union (namesExpr a) (namesExpr b)

namesExprMaybe :: Maybe (Expr a) -> Set String
namesExprMaybe = maybe Set.empty namesExpr

namesIdents :: [Ident a] -> Set String
namesIdents = Set.unions . map namesIdent

namesIdentMaybe :: Maybe (Ident a) -> Set String
namesIdentMaybe = maybe Set.empty namesIdent

namesIdent :: Ident a -> Set String
namesIdent = Set.singleton . ident_string

namesArguments :: [Argument a] -> Set String
namesArguments = Set.unions . map namesArgument

namesArgument :: Argument a -> Set String
namesArgument a@ArgKeyword{} = Set.insert (ident_string $ arg_keyword a) . namesExpr $ arg_expr a
namesArgument a              = namesExpr $ arg_expr a

namesParameters :: [Parameter a] -> Set String
namesParameters = Set.unions . map namesParameter

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
namesCompFor c = Set.unions [inExpr, forIter, forExprs]
  where inExpr   = namesExpr $ comp_in_expr c
        forIter  = namesCompIterMaybe $ comp_for_iter c
        forExprs = namesExprs $ comp_for_exprs c

namesCompIter :: CompIter a -> Set String
namesCompIter c@IterFor{} = namesCompFor $ comp_iter_for c
namesCompIter c@IterIf{}  = namesCompIf $ comp_iter_if c

namesCompIterMaybe :: Maybe (CompIter a) -> Set String
namesCompIterMaybe = maybe Set.empty namesCompIter

namesCompIf :: CompIf a -> Set String
namesCompIf c = Set.union (namesExpr $ comp_if c) (namesCompIterMaybe $ comp_if_iter c)
