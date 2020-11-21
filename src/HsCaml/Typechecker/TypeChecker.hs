module HsCaml.TypeChecker.TypeChecker where

import Debug.Trace
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.CheckFinished
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.UFTypeChecker

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  renameSymsByScope e >>= uftypeCheck >>= checkFinished

--let te' = initialTypeInfer e'
---- traceM $ show $ te'
--let constraints = collectTypeConstraints te'
---- traceM $ show $ constraints
--unified <- unify constraints
---- traceM $ show $ unified
--let typevarReplaced = setTypeConstraints unified te'
--checkTypeCheckIsFinished typevarReplaced
