module Simplifier where

import Language.Python.Common.AST
import Language.Python.Common.Pretty (pretty, render)
import Language.Python.Common.PrettyAST ()
import Language.Python.Version3.Parser (parseExpr)

import Names (namesExpr)
