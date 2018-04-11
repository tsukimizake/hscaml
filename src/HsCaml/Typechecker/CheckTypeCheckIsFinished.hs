{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module HsCaml.TypeChecker.CheckTypeCheckIsFinished (checkTypeCheckIsFinished) where

import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckUtil
import Control.Lens.Operators
import Data.Text
import Data.Monoid
checkTypeCheckIsFinished :: TExpr -> Either CompileError TExpr
checkTypeCheckIsFinished = traverseTExpr impl
  where
    isCorrectlyInferedType :: TypeExpr -> Bool
    isCorrectlyInferedType (TypeVar _) = False
    isCorrectlyInferedType UnspecifiedType = False
    isCorrectlyInferedType _ = True
    impl :: TExpr -> Either CompileError TExpr
    impl e = let isOK = isCorrectlyInferedType $ e ^. _typeExpr
             in if isOK
                then Right e
                else Left . TypeError $ (pack . show) (e ^. _typeExpr) <> " couldn't be infered!\n in Expr" <> (pack . show) (toExpr e)
