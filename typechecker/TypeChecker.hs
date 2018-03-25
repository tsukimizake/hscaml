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

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  let e' = renameSymsByScope e
  let te' = initialTypeInfer e'
  let constraints = collectTypeConstraints te'
  unified <- unify constraints
  let res = setTypeConstraints unified te'
  pure res

traceTexpr :: String -> IO ()
traceTexpr s = do
    let expr = renameSymsByScope . parseExpr $ s
    let texpr = initialTypeInfer expr
    print texpr
    let constraints = collectTypeConstraints texpr
    print constraints
