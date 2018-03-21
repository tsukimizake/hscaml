{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module TypeChecker where

import Types
import OCamlType
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.State
import Data.Text
import Data.Monoid
import TypeCheckUtil
import Data.Map as M
import Data.Maybe
import Data.Set as S
import Debug.Trace
import Parser
import RenameSymsByScope
import InitialTypeInfer
import CollectTypeConstraints

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  let e' = renameSymsByScope e
  let te' = initialTypeInfer e'
  let constraints = collectTypeConstraints te'
  pure te'

traceTexpr :: String -> IO ()
traceTexpr s = do
    let expr = renameSymsByScope . parseExpr $ s
    let texpr = initialTypeInfer expr
    print texpr
    let constraints = collectTypeConstraints texpr
    print constraints
