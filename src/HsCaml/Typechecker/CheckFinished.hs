{-# OPTIONS -Wall #-}

module HsCaml.TypeChecker.CheckFinished (checkFinished) where

import Control.Lens.Operators
import Data.Text
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckUtil

checkFinished :: TExpr -> Either CompileError TExpr
checkFinished = traverseTExpr impl
  where
    isCorrectlyInferedType :: TypeExpr -> Bool
    isCorrectlyInferedType UnspecifiedType = False
    isCorrectlyInferedType _ = True
    impl :: TExpr -> Either CompileError TExpr
    impl e =
      let isOK = isCorrectlyInferedType $ e ^. typeExpr_
       in if isOK
            then Right e
            else Left . TypeError $ (pack . show) (e ^. typeExpr_) <> " couldn't be infered!\n in Expr " <> (pack . show) (toExpr e)
