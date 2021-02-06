{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# OPTIONS -Wall #-}

module HsCaml.TypeChecker.CheckFinished (checkFinished) where

import Control.Lens.Operators
import Data.Text
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckUtil

checkFinished :: Expr -> Either CompileError Expr
checkFinished = traverseExpr impl
  where
    isCorrectlyInferedType :: TypeExpr -> Bool
    isCorrectlyInferedType UnspecifiedType = False
    isCorrectlyInferedType _ = True
    impl :: Expr -> Either CompileError Expr
    impl e =
      let isOK = isCorrectlyInferedType $ e.typeExpr
       in if isOK
            then Right e
            else Left . TypeError $ (pack . show) (e.typeExpr) <> " couldn't be infered!\n in Expr " <> (pack . show) e
