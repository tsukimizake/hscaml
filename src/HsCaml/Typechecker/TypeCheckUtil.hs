{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module HsCaml.TypeChecker.TypeCheckUtil where

import Control.Monad.Identity
import Data.Bifunctor
import Data.Text
import HsCaml.FrontEnd.Types as Types

traverseExpr :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExpr f x@(Constant _ _) = f x
traverseExpr f x@(Var _ _) = f x
traverseExpr f (Paren e t) = do
  e' <- traverseExpr f e
  f $ Paren e' t
traverseExpr f (InfixOpExpr e iop g t) = do
  e' <- traverseExpr f e
  g' <- traverseExpr f g
  f $ InfixOpExpr e' iop g' t
traverseExpr f (BegEnd e t) = do
  e' <- traverseExpr f e
  f $ BegEnd e' t
traverseExpr f (MultiExpr e t) = do
  e' <- mapM f e
  f $ MultiExpr e' t
traverseExpr f (Constr e t) = do
  e' <- traverseExpr f e
  f $ Constr e' t
traverseExpr f (IfThenElse e1 e2 e3 t) = do
  e1' <- traverseExpr f e1
  e2' <- traverseExpr f e2
  e3' <- traverseExpr f e3
  f $ IfThenElse e1' e2' e3' t
traverseExpr f (Match e1 e2 t) = do
  e1' <- traverseExpr f e1
  e2' <-
    mapM
      ( \(x, y) -> do
          y' <- traverseExpr f y
          pure (x, y')
      )
      e2
  f $ Match e1' e2' t
traverseExpr f (While e1 e2 t) = do
  e1' <- traverseExpr f e1
  e2' <- traverseExpr f e2
  f $ While e1' e2' t
traverseExpr f (FunApply e1 e2 t) = do
  e1' <- traverseExpr f e1
  e2' <- mapM f e2
  f $ FunApply e1' e2' t
traverseExpr f (Let e1 e2 t) = do
  e2' <- traverseExpr f e2
  f $ Let e1 e2' t
traverseExpr f (LetRec e1 e2 t) = do
  e2' <- traverseExpr f e2
  f $ LetRec e1 e2' t
traverseExpr f (LetIn e1 e2 e3 t) = do
  e2' <- traverseExpr f e2
  e3' <- traverseExpr f e3
  f $ LetIn e1 e2' e3' t
traverseExpr f (LetRecIn e1 e2 e3 t) = do
  e2' <- traverseExpr f e2
  e3' <- traverseExpr f e3
  f $ LetRecIn e1 e2' e3' t
traverseExpr f (Types.List xs t) = do
  xs' <- mapM (traverseExpr f) xs
  f $ Types.List xs' t
traverseExpr f (Array xs t) = do
  xs' <- mapM (traverseExpr f) xs
  f $ Array xs' t

traverseTypeExpr :: (Monad m) => (TypeExpr -> m TypeExpr) -> (TypeExpr -> m TypeExpr)
traverseTypeExpr f (l ::-> r) = do
  l' <- traverseTypeExpr f l
  r' <- traverseTypeExpr f r
  f (l' ::-> r')
traverseTypeExpr f (l ::* r) = do
  l' <- traverseTypeExpr f l
  r' <- traverseTypeExpr f r
  f (l' ::* r')
traverseTypeExpr f (l ::+ r) = do
  l' <- traverseTypeExpr f l
  r' <- traverseTypeExpr f r
  f (l' ::+ r')
traverseTypeExpr f (ParenTypeExpr inner) = f inner
traverseTypeExpr f (TypeApplication xs a) = do
  xs' <- mapM (traverseTypeExpr f) xs
  a' <- traverseTypeExpr f a
  f $ TypeApplication xs' a'
traverseTypeExpr f x = f x

newtype TV = TV Text deriving (Show, Eq, Ord)

class TypeVarReplaceable a where
  replaceTypeVar :: TV -> TypeExpr -> a -> a

instance TypeVarReplaceable TypeExpr where
  replaceTypeVar from to orig = runIdentity $ traverseTypeExpr impl orig
    where
      impl :: TypeExpr -> Identity TypeExpr
      impl orig@(TypeVar _ l) = if fromTV from l == orig then pure to else pure orig
      impl te = pure te

data TypeConstraint = TypeEq
  { _lhs_ :: TypeExpr,
    _rhs_ :: TypeExpr
  }
  deriving (Ord, Eq)

instance Show TypeConstraint where
  show (TypeEq l r) = "(TypeEq (" <> show l <> ") (" <> show r <> "))"

instance TypeVarReplaceable TypeConstraint where
  replaceTypeVar from to (TypeEq l r) = TypeEq (replaceTypeVar from to l) (replaceTypeVar from to r)

instance TypeVarReplaceable Pattern where
  replaceTypeVar from to (ConstantPattern t v) = ConstantPattern (replaceTypeVar from to t) v
  replaceTypeVar from to (ParenPattern t p) = ParenPattern (replaceTypeVar from to t) (replaceTypeVar from to p)
  replaceTypeVar from to (ListPattern t v) = ListPattern (replaceTypeVar from to t) (fmap (replaceTypeVar from to) v)
  replaceTypeVar from to (VarPattern t s) = VarPattern (replaceTypeVar from to t) s
  replaceTypeVar from to (OrPattern t l r) = undefined

instance TypeVarReplaceable LetPattern where
  replaceTypeVar from to (FuncLetPattern t f xs) = FuncLetPattern (replaceTypeVar from to t) f (fmap (replaceTypeVar from to) <$> xs)
  replaceTypeVar from to (LetPattern t s) = LetPattern (replaceTypeVar from to t) (replaceTypeVar from to s)

instance TypeVarReplaceable Expr where
  replaceTypeVar from@(TV fs) to orig = runIdentity $ (traverseExpr $ pure . impl) orig
    where
      replaceInPatterns (Match a xs b) = Match a (fmap (first (replaceTypeVar from to)) xs) b
      replaceInPatterns (Let p a b) = Let (replaceTypeVar from to p) a b
      replaceInPatterns (LetIn p a b c) = LetIn (replaceTypeVar from to p) a b c
      replaceInPatterns (LetRec p a b) = LetRec (replaceTypeVar from to p) a b
      replaceInPatterns (LetRecIn p a b c) = LetRecIn (replaceTypeVar from to p) a b c
      replaceInPatterns x = x
      impl :: Expr -> Expr
      impl e =
        replaceInPatterns $ e {typeExpr = replaceTypeVar from to e.typeExpr}

toTV :: TypeExpr -> Either CompileError TV
toTV (TypeVar s _) = pure $ TV s
toTV v = Left $ TypeError $ pack $ show v <> "is not TypeVar"

fromTV :: TV -> Types.Level -> TypeExpr
fromTV (TV s) = TypeVar s
