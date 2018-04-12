{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HsCaml.TypeChecker.Unify (unify, traverseTypeExpr, TypeVarReplaceable, replaceTypeVar) where
import HsCaml.FrontEnd.Types hiding (TV)
import HsCaml.TypeChecker.CollectTypeConstraints
import Data.Set as S
import Control.Monad
import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Text
import Data.Monoid
import HsCaml.TypeChecker.TypeCheckUtil
import Data.Functor.Identity
import Control.Monad.State
import Debug.Trace
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

repeatToFix :: (Eq a, Monad f) => (a->f a) -> a -> f a
repeatToFix f x = do
  y <- f x
  if x==y
    then pure y
    else repeatToFix f y

data UnifyMState = UnifyMState
  {
    __constraints :: [TypeConstraint],
    __reservedRewrites :: [(TV, TypeExpr)]
  }deriving (Show)
L.makeLenses ''UnifyMState

newtype UnifyM a = UnifyM (StateT UnifyMState (Either CompileError) a) deriving (Functor, Applicative, Monad)

runUnifyM :: UnifyM a -> Either CompileError [TypeConstraint]
runUnifyM (UnifyM prog) = do
  res <- (impl `execStateT` (UnifyMState [] []))
  pure $ res ^. _constraints
  where
    impl :: StateT UnifyMState (Either CompileError) [TypeConstraint]
    impl = do
      _<-prog
      rewrites <- L.use _reservedRewrites
      forM_ rewrites $ \(l, r) -> do
        _constraints %= fmap (replaceTypeVar l r)
        _constraints %= (TypeEq (fromTV l) r :)
      L.use _constraints

putUnifiedConstraint :: TypeConstraint -> UnifyM ()
putUnifiedConstraint tc = UnifyM $ _constraints %= (tc:)

reserveReplaceTypeVar :: TV -> TypeExpr -> UnifyM ()
reserveReplaceTypeVar from to = UnifyM $ _reservedRewrites %= ((from, to):)

liftUnifyM :: Either CompileError a -> UnifyM a
liftUnifyM = UnifyM . lift

unify :: Set TypeConstraint -> Either CompileError (Set TypeConstraint)
unify cs =
  let fvs = S.toList . collectTypeVar . S.toList $ cs
  in fmap S.fromList $ (runUnifyM . (repeatToFix $ unifyimpl fvs) . S.toList $ cs)

unifyimpl :: [TV] -> [TypeConstraint] -> UnifyM [TypeConstraint]
unifyimpl _ [] = pure []
unifyimpl fv (TypeEq (l1 ::-> l2) (r1 ::-> r2) : rest) = unifyimpl fv (TypeEq l2 r2 : TypeEq l1 r1 : rest)
unifyimpl fv (TypeEq l r : rest)
  | l == r = unifyimpl fv rest
  | isTypeVar l = do
      l' <- liftUnifyM $ toTV l :: UnifyM TV
      rest' <- unifyimpl fv (fmap (replaceTypeVar l' r) rest)
      reserveReplaceTypeVar l' r
      putUnifiedConstraint $ TypeEq l r
      pure $ (fmap (replaceTypeVar l' r) rest')
  | (not (isTypeVar l)) && isTypeVar r = unifyimpl fv (TypeEq r l : rest) -- swap
  | otherwise = liftUnifyM $ Left $ TypeError $ pack $ "unify fail \nconstraints : " <> show l <> " = " <> show r