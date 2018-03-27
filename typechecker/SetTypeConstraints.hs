{-# OPTIONS -Wall #-}
module SetTypeConstraints (setTypeConstraints) where
import Types
import CollectTypeConstraints
import Data.Set as S
import TypeCheckUtil
import Control.Lens.Operators
import Control.Lens as L
import Debug.Trace

setTypeConstraints :: S.Set TypeConstraint -> TExpr -> TExpr
setTypeConstraints cs te = S.foldr impl te cs


impl :: TypeConstraint -> TExpr -> TExpr
impl (TypeEq (TypeVar ls) r) te = replaceTypeVar (TV ls) r te
impl (TypeEq l (TypeVar rs)) te = replaceTypeVar (TV rs) l te
impl _ te = te
