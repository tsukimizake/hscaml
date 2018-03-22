{-# OPTIONS -Wall #-}
module Unify where
import Types
import CollectTypeConstraints
import Data.Set as S

collectTypeVar :: S.Set TypeConstraint -> S.Set TypeExpr
collectTypeVar S.empty = S.empty
collectTypeVar c = do
  constraint <- c
  empty
