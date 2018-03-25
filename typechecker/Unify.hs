{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Unify (unify, traverseTypeExpr, TypeVarReplaceable, replaceTypeVar) where
import Types hiding (TV)
import CollectTypeConstraints
import Data.Set as S
import Control.Monad
import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Text
import Data.Monoid
import TypeCheckUtil
import Data.Functor.Identity
import Control.Monad.State

-- chooseOne [a, b, c]
-- >> [(a, [b, c]), (b, [a, c]), (c, [a, b])]
chooseOne :: [a] -> [(a, [a])]
chooseOne [] = []
chooseOne (x:xs) = (x, xs) : (fmap (\(y, ys) -> (y,(x:ys))) $ chooseOne xs)

isTypeVar :: TypeExpr -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

collectTypeVar :: [TypeConstraint] -> S.Set TV
collectTypeVar = S.fromList . join . fmap impl
  where
    impl :: TypeConstraint -> [TV]
    impl (TypeEq l r) = f l ++ f r
    f (TypeVar s) = [TV s]
    f _ = []


containsTypeVar :: TV -> TypeExpr -> Bool
containsTypeVar tv te = execState (traverseTypeExpr impl te) False
  where
    impl :: TypeExpr -> State Bool TypeExpr
    impl te@(TypeVar _) = do
      if (fromTV tv == te)
        then put True
        else pure()
      pure te
    impl x = pure x

unify :: Set TypeConstraint -> Either CompileError (Set TypeConstraint)
unify cs =
  let fvs = S.toList . collectTypeVar . S.toList $ cs
      allTVsAreSolved = undefined
  in fmap S.fromList $ (unifyimpl fvs) . S.toList $ cs
  where
    isTEq (TypeEq _ _) = True
    isTEq _ = False
    unifyimpl :: [TV] -> [TypeConstraint] -> Either CompileError [TypeConstraint]
    unifyimpl _ [] = pure []
    unifyimpl fv (TypeEq (l1 ::-> l2) (r1 ::-> r2) : rest) = unifyimpl fv (TypeEq l2 r2 : TypeEq l1 r1 : rest)
    unifyimpl fv (TypeEq l r : rest)
      | l == r = unifyimpl fv rest
      | isTypeVar l = do
          l' <- toTV l
          let rest' = fmap (replaceTypeVar l' r) rest
          rest'' <- unifyimpl fv (rest')
          pure $ TypeEq l r : rest''
      | not (isTypeVar l) && isTypeVar r = unifyimpl fv (TypeEq r l : rest) -- swap
      | otherwise = Left $ TypeError $ pack $ "unify fail fvs: " <> show fv <> "\nconstraints : " <> show cs
