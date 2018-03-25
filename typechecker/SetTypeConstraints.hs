{-# OPTIONS -Wall #-}
module SetTypeConstraints (setTypeConstraints) where
import Types
import CollectTypeConstraints
import Data.Set as S
import TypeCheckUtil
import Control.Lens.Operators
import Control.Lens as L
import Unify

setTypeConstraints :: S.Set TypeConstraint -> TExpr -> TExpr
setTypeConstraints cs te = S.foldr impl te cs


impl :: TypeConstraint -> TExpr -> TExpr
impl cste te = undefined
