module HsCaml.TypeChecker.UFTypeChecker where

import Data.Map as M
import Debug.Trace
import HsCaml.FrontEnd.Types

uftypeCheck :: Expr -> Either CompileError TExpr
uftypeCheck e = traceShowM e >>= undefined
