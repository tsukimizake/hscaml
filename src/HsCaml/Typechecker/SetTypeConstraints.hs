{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module HsCaml.TypeChecker.SetTypeConstraints (applyGamma) where
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.CollectTypeConstraints
import HsCaml.TypeChecker.TypeCheckUtil


class GammaAppliable a where
  applyGamma :: [TypeConstraint] -> a -> a

instance GammaAppliable TExpr where
  applyGamma gm te = foldr impl te gm

impl :: (TypeVarReplaceable a) => TypeConstraint -> a -> a
impl (TypeEq (TypeVar ls) r) te = replaceTypeVar (TV ls) r te
impl (TypeEq l (TypeVar rs)) te = replaceTypeVar (TV rs) l te
impl _ te = te

instance GammaAppliable Pattern where
  applyGamma gm pat = foldr impl pat gm

instance GammaAppliable LetPattern where
  applyGamma gm pat = foldr impl pat gm

instance GammaAppliable [TypeConstraint] where
  applyGamma gm g = foldr impl g gm
