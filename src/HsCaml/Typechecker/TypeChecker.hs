module HsCaml.TypeChecker.TypeChecker where

import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.InitialTypeInfer
import HsCaml.TypeChecker.CollectTypeConstraints
import HsCaml.TypeChecker.Unify
import HsCaml.TypeChecker.SetTypeConstraints
import HsCaml.TypeChecker.CheckTypeCheckIsFinished

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  e' <- renameSymsByScope e
  let te' = initialTypeInfer e'
  -- traceM $ show $ te'
  let constraints = collectTypeConstraints te'
  -- traceM $ show $ constraints
  unified <- unify constraints
  -- traceM $ show $ unified
  let typevarReplaced = setTypeConstraints unified te'
  checkTypeCheckIsFinished typevarReplaced
