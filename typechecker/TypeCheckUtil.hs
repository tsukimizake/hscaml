{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeCheckUtil where
import Types
import Data.Text
import Data.Functor.Identity
import Data.Monoid
import Control.Lens as L

-- Exprに対するmap
traverseExpr :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExpr f x@(Constant _) = f x
traverseExpr f x@(Var _) = f x
traverseExpr f (Paren e) = do
    e'  <- traverseExpr f e
    f $ Paren e'
traverseExpr f (InfixOpExpr e iop g) = do
    e'  <- traverseExpr f e
    g'  <- traverseExpr f g
    f $ InfixOpExpr e' iop g'
traverseExpr f (BegEnd e) = do
    e'  <- traverseExpr f e
    f $ BegEnd e'
traverseExpr f (MultiExpr e) = do
    e' <- mapM f e
    f $ MultiExpr e'
traverseExpr f (Constr e) = do
    e'  <- traverseExpr f e
    f $ Constr e'
traverseExpr f (IfThenElse e1 e2 e3) = do
    e1'  <- traverseExpr f e1
    e2'  <- traverseExpr f e2
    e3'  <- traverseExpr f e3
    f $ IfThenElse e1' e2' e3'
traverseExpr f (Match e1 e2) = do
    e1'  <- traverseExpr f e1
    e2' <- mapM (\(x, y) -> do
                     y'  <- traverseExpr f y
                     pure (x, y')) e2
    f $ Match e1' e2'
traverseExpr f (While e1 e2) = do
    e1'  <- traverseExpr f e1
    e2'  <- traverseExpr f e2
    f $ While e1' e2'
traverseExpr f (FunApply e1 e2) = do
    e1'  <- traverseExpr f e1
    e2' <- mapM f e2
    f $ FunApply e1' e2'
traverseExpr f (Let e1 e2) =  do
    e2'  <- traverseExpr f e2
    f $ Let e1 e2'
traverseExpr f (LetRec e1 e2) =  do
    e2'  <- traverseExpr f e2
    f $ LetRec e1 e2'
traverseExpr f (LetIn e1 e2 e3) = do
    e2'  <- traverseExpr f e2
    e3'  <- traverseExpr f e3
    f $ LetIn e1 e2' e3'
traverseExpr _ (TypeDecl e1 e2) = pure $ TypeDecl e1 e2

traverseTExpr :: (Monad m) => (TExpr -> m TExpr) -> TExpr -> m TExpr
traverseTExpr f x@(TConstant _ _) = f x
traverseTExpr f x@(TVar _ _) = f x
traverseTExpr f (TParen e t) = do
    e'  <- traverseTExpr f e
    f $ TParen e' t
traverseTExpr f (TInfixOpExpr e iop g t) = do
    e'  <- traverseTExpr f e
    g'  <- traverseTExpr f g
    f $ TInfixOpExpr e' iop g' t
traverseTExpr f (TBegEnd e t) = do
    e'  <- traverseTExpr f e
    f $ TBegEnd e' t
traverseTExpr f (TMultiExpr e t) = do
    e' <- mapM f e
    f $ TMultiExpr e' t
traverseTExpr f (TConstr e t) = do
    e'  <- traverseTExpr f e
    f $ TConstr e' t
traverseTExpr f (TIfThenElse e1 e2 e3 t) = do
    e1'  <- traverseTExpr f e1
    e2'  <- traverseTExpr f e2
    e3'  <- traverseTExpr f e3
    f $ TIfThenElse e1' e2' e3' t
traverseTExpr f (TMatch e1 e2 t) = do
    e1'  <- traverseTExpr f e1
    e2' <- mapM (\(x, y) -> do
                     y'  <- traverseTExpr f y
                     pure (x, y')) e2
    f $ TMatch e1' e2' t
traverseTExpr f (TWhile e1 e2 t) = do
    e1'  <- traverseTExpr f e1
    e2'  <- traverseTExpr f e2
    f $ TWhile e1' e2' t
traverseTExpr f (TFunApply e1 e2 t) = do
    e2' <- mapM f e2
    f $ TFunApply e1 e2' t
traverseTExpr f (TLet e1 e2 t) =  do
    e2'  <- traverseTExpr f e2
    f $ TLet e1 e2' t
traverseTExpr f (TLetRec e1 e2 t) =  do
    e2'  <- traverseTExpr f e2
    f $ TLetRec e1 e2' t
traverseTExpr f (TLetIn e1 e2 e3 t) = do
    e2'  <- traverseTExpr f e2
    e3'  <- traverseTExpr f e3
    f $ TLetIn e1 e2' e3' t
traverseTExpr f (TTypeDecl e1 e2 t) = do
  f $ TTypeDecl e1 e2 t

traverseTypeExpr :: (Monad m) => (TypeExpr -> m TypeExpr) -> TypeExpr -> (m TypeExpr)
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

data TV = TV Text deriving (Show, Eq, Ord)

class TypeVarReplaceable a where
  replaceTypeVar :: TV -> TypeExpr -> a -> a

instance TypeVarReplaceable TypeExpr where
  replaceTypeVar from to orig = runIdentity $ traverseTypeExpr impl orig
    where
      impl :: TypeExpr -> Identity TypeExpr
      impl orig@(TypeVar _) = if (fromTV from) == orig then pure to else pure orig
      impl te = pure te

data TypeConstraint =
  TypeEq{
  __lhs :: TypeExpr,
  __rhs :: TypeExpr
  } deriving (Show, Ord, Eq)


instance TypeVarReplaceable TypeConstraint where
  replaceTypeVar from to (TypeEq l r) = TypeEq (replaceTypeVar from to l) (replaceTypeVar from to r)

instance TypeVarReplaceable TExpr where
  replaceTypeVar from@(TV fs) to orig = runIdentity $ (traverseTExpr $ pure . impl) orig
    where
      impl :: TExpr -> TExpr
      impl e = e & _typeExpr .~ (replaceTypeVar from to (e ^. _typeExpr))

toTV :: TypeExpr -> Either CompileError TV
toTV (TypeVar s) = pure $ TV s
toTV v = Left $ TypeError $ pack $ show v <> "is not TypeVar"

fromTV :: TV -> TypeExpr
fromTV (TV s) = TypeVar s

L.makeLenses ''TypeConstraint
