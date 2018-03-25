{-# OPTIONS -Wall #-}
module SetTypeConstraints (setTypeConstraints) where
import Types
import CollectTypeConstraints
import Data.Set as S
import TypeCheckUtil
import Control.Lens.Operators
import Control.Lens as L
setTypeConstraints :: S.Set TypeConstraint -> TExpr -> TExpr
setTypeConstraints cs te = S.foldr impl te cs

impl :: TypeConstraint -> TExpr -> TExpr
impl (TypeOfExpr cste) te = (traverseTExpr (\e -> pure $ go e)) te cste
  where
    go :: TExpr -> TExpr
    go te =
      if (te ^. L.re _TExpr :: Expr) == (cste ^. L.re _TExpr :: Expr)
      then te & _typeExpr .~ (cste ^. _typeExpr)
      else te
    -- TExprのExpr部分が同じだったらTrue
    exprEqual :: TExpr -> TExpr -> Bool
    exprEqual l r = (l & _typeExpr .~ (r ^. _typeExpr)) == r
impl _ te = te
