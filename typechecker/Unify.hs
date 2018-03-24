{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Unify (unify) where
import Types hiding (TVar)
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

data TVar = TVar Text deriving(Show, Eq, Ord)

-- chooseOne [a, b, c]
-- >> [(a, [b, c]), (b, [a, c]), (c, [a, b])]
chooseOne :: [a] -> [(a, [a])]
chooseOne [] = []
chooseOne (x:xs) = (x, xs) : (fmap (\(y, ys) -> (y,(x:ys))) $ chooseOne xs)

isTypeVar :: TypeExpr -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

collectTypeVar :: [TypeConstraint] -> S.Set TVar
collectTypeVar = S.fromList . join . fmap impl
  where
    impl :: TypeConstraint -> [TVar]
    impl (TypeEq l r) = f l ++ f r
    impl (TypeOfExpr e) = f (e ^. _typeExpr)
    f (TypeVar s) = [TVar s]
    f _ = []

traverseTypeExpr :: (Monad m) => (TypeExpr -> m TypeExpr) -> TypeExpr -> (m TypeExpr)
traverseTypeExpr f (l ::-> r) = do
  l' <- traverseTypeExpr f l
  r' <- traverseTypeExpr f r
  pure (l' ::-> r')
traverseTypeExpr f (l ::* r) = do
  l' <- traverseTypeExpr f l
  r' <- traverseTypeExpr f r
  pure (l' ::* r')
traverseTypeExpr f (l ::+ r) = do
  l' <- traverseTypeExpr f l
  r' <- traverseTypeExpr f r
  pure (l' ::+ r')
traverseTypeExpr f (ParenTypeExpr inner) = f inner
traverseTypeExpr f (TypeApplication xs a) = do
  xs' <- mapM (traverseTypeExpr f) xs
  a' <- traverseTypeExpr f a
  pure $ TypeApplication xs' a'
traverseTypeExpr f x = f x

containsTypeVar :: TVar -> TypeExpr -> Bool
containsTypeVar tv te = execState (traverseTypeExpr impl te) False
  where
    impl :: TypeExpr -> State Bool TypeExpr
    impl te@(TypeVar _) = do
      if (fromTVar tv == te)
        then put True
        else pure()
      pure te
    impl x = pure x

class TypeVarReplaceable a where
  replaceTypeVar :: TVar -> TypeExpr -> a -> a

instance TypeVarReplaceable TypeExpr where
  replaceTypeVar from to orig = runIdentity $ traverseTypeExpr impl orig
    where
      impl :: TypeExpr -> Identity TypeExpr
      impl orig@(TypeVar _) = if (fromTVar from) == orig then pure to else pure orig
      impl te = pure te

instance TypeVarReplaceable TypeConstraint where
  replaceTypeVar from to (TypeEq l r) = TypeEq (replaceTypeVar from to l) (replaceTypeVar from to r)
  replaceTypeVar from to (TypeOfExpr txpr) = TypeOfExpr $ runIdentity $ mapMTExpr impl txpr
    where
      impl :: TExpr -> Identity TExpr
      impl e = let texpr =  e ^. _typeExpr
                   newtexpr = replaceTypeVar from to texpr
               in pure $ e & _typeExpr .~ newtexpr

toTVar :: TypeExpr -> Either CompileError TVar
toTVar (TypeVar s) = pure $ TVar s
toTVar v = Left $ TypeError $ pack $ show v <> "is not TypeVar"

fromTVar :: TVar -> TypeExpr
fromTVar (TVar s) = TypeVar s

unify :: Set TypeConstraint -> Either CompileError (Set TypeConstraint)
unify cs =
  let fvs = S.toList . collectTypeVar . S.toList $ cs
      allTVarsAreSolved = undefined
  in fmap S.fromList $ (unifyimpl fvs) . S.toList $ cs
  where
    isTEq (TypeEq _ _) = True
    isTEq _ = False
    unifyimpl :: [TVar] -> [TypeConstraint] -> Either CompileError [TypeConstraint]
    unifyimpl _ [] = pure []
    unifyimpl fv (t@(TypeOfExpr _) : rest) =
      if Prelude.all (not . isTEq) rest
      then pure $ t:rest
      else unifyimpl fv (rest++[t])
    unifyimpl fv (TypeEq (l1 ::-> l2) (r1 ::-> r2) : rest) = unifyimpl fv (TypeEq l2 r2 : TypeEq l1 r1 : rest)
    unifyimpl fv (TypeEq l r : rest)
      | l == r = unifyimpl fv rest
      | isTypeVar l = do
          l' <- toTVar l
          let rest' = fmap (replaceTypeVar l' r) rest
          rest'' <- unifyimpl fv (rest')
          pure $ TypeEq l r : rest''
      | not (isTypeVar l) && isTypeVar r = unifyimpl fv (TypeEq r l : rest) -- swap
      | otherwise = Left $ TypeError $ pack $ "unify fail fvs: " <> show fv <> "\nconstraints : " <> show cs
