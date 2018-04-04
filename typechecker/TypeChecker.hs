{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module TypeChecker where

import Types
import Parser
import RenameSymsByScope
import InitialTypeInfer
import CollectTypeConstraints
import Unify
import SetTypeConstraints
import Debug.Trace
import CheckTypeCheckIsFinished

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  let e' = renameSymsByScope e
  let te' = initialTypeInfer e'
  -- traceM $ show $ te'
  let constraints = collectTypeConstraints te'
  -- traceM $ show $ constraints
  unified <- unify constraints
  -- traceM $ show $ unified
  let typevarReplaced = setTypeConstraints unified te'
  checkTypeCheckIsFinished typevarReplaced

traceTexpr :: String -> IO ()
traceTexpr s = do
    let expr = renameSymsByScope . parseExpr $ s
    let texpr = initialTypeInfer expr
    print texpr
    let constraints = collectTypeConstraints texpr
    print constraints
