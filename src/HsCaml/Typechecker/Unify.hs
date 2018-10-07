{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HsCaml.TypeChecker.Unify (unify, traverseTypeExpr, TypeVarReplaceable, replaceTypeVar) where
import HsCaml.FrontEnd.Types 
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
import Data.List

isTypeVar :: TypeExpr -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

containsTypeVar :: TV -> TypeExpr -> Bool
containsTypeVar tv te = execState (traverseTypeExpr impl te) False
  where
    impl :: TypeExpr -> State Bool TypeExpr
    impl te@(TypeVar _) = do
      if fromTV tv == te
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
  } deriving (Show)
L.makeLenses ''UnifyMState

newtype UnifyM a = UnifyM (StateT UnifyMState (Either CompileError) a) deriving (Functor, Applicative, Monad)

runUnifyM :: UnifyM a -> Either CompileError [TypeConstraint]
runUnifyM (UnifyM prog) = do
  res <- impl `execStateT` (UnifyMState [] [])
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

unify :: [TypeConstraint] -> Either CompileError [TypeConstraint]
unify cs =
  let fvs = getTypeVar cs
   in nub <$> (runUnifyM . repeatToFix (unifyImpl fvs) $ cs)

unifyImpl :: [TV] -> [TypeConstraint] -> UnifyM [TypeConstraint]
unifyImpl _ [] = pure []
unifyImpl fv (TypeEq (l1 ::-> l2) (r1 ::-> r2) : rest) = unifyImpl fv (TypeEq l2 r2 : TypeEq l1 r1 : rest)
unifyImpl fv (TypeEq l r : rest)
  | l == r = unifyImpl fv rest
  | isTypeVar l = do
      l' <- liftUnifyM $ toTV l :: UnifyM TV
      rest' <- unifyImpl fv (fmap (replaceTypeVar l' r) rest)
      reserveReplaceTypeVar l' r
      putUnifiedConstraint $ TypeEq l r
      pure $ fmap (replaceTypeVar l' r) rest'
  | (not (isTypeVar l)) && isTypeVar r = unifyImpl fv (TypeEq r l : rest) -- swap
  | otherwise = liftUnifyM $ Left $ TypeError $ pack $ "unify fail \nconstraints : " <> show l <> " = " <> show r
unifyImpl fv e@(TypeOfExpr xs s t : rest) = do
  rest' <- unifyImpl fv rest
  putUnifiedConstraint $ TypeOfExpr xs s t
  pure $ TypeOfExpr xs s t : rest' 
