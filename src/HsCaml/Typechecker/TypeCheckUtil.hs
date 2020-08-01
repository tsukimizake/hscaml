module HsCaml.TypeChecker.TypeCheckUtil where
import HsCaml.FrontEnd.Types as Types
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
    pure $ Paren e'
traverseExpr f (InfixOpExpr e iop g) = do
    e'  <- traverseExpr f e
    g'  <- traverseExpr f g
    pure $ InfixOpExpr e' iop g'
traverseExpr f (BegEnd e) = do
    e'  <- traverseExpr f e
    pure $ BegEnd e'
traverseExpr f (MultiExpr e) = do
    e' <- mapM f e
    pure $ MultiExpr e'
traverseExpr f (Constr e) = do
    e'  <- traverseExpr f e
    pure $ Constr e'
traverseExpr f (IfThenElse e1 e2 e3) = do
    e1'  <- traverseExpr f e1
    e2'  <- traverseExpr f e2
    e3'  <- traverseExpr f e3
    pure $ IfThenElse e1' e2' e3'
traverseExpr f (Match e1 e2) = do
    e1'  <- traverseExpr f e1
    e2' <- mapM (\(x, y) -> do
                    y'  <- traverseExpr f y
                    pure (x, y')) e2
    pure $ Match e1' e2'
traverseExpr f (While e1 e2) = do
    e1'  <- traverseExpr f e1
    e2'  <- traverseExpr f e2
    pure $ While e1' e2'
traverseExpr f (FunApply e1 e2) = do
    e1'  <- traverseExpr f e1
    e2' <- mapM f e2
    pure $ FunApply e1' e2'
traverseExpr f (Let e1 e2) =  do
    e2'  <- traverseExpr f e2
    pure $ Let e1 e2'
traverseExpr f (LetRec e1 e2) =  do
    e2'  <- traverseExpr f e2
    pure $ LetRec e1 e2'
traverseExpr f (LetIn e1 e2 e3) = do
    e2'  <- traverseExpr f e2
    e3'  <- traverseExpr f e3
    pure $ LetIn e1 e2' e3'
traverseExpr f (LetRecIn e1 e2 e3) = do
    e2'  <- traverseExpr f e2
    e3'  <- traverseExpr f e3
    pure $ LetRecIn e1 e2' e3'
traverseExpr f (Types.List xs) = do
  xs' <- mapM (traverseExpr f) xs
  pure $ Types.List xs'
traverseExpr f (Array xs) = do
  xs' <- mapM (traverseExpr f) xs
  pure $ Array xs'

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
    e1' <- traverseTExpr f e1
    e2' <- mapM f e2
    f $ TFunApply e1' e2' t
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
traverseTExpr f (TLetRecIn e1 e2 e3 t) = do
    e2'  <- traverseTExpr f e2
    e3'  <- traverseTExpr f e3
    f $ TLetRecIn e1 e2' e3' t
traverseTExpr f (Types.TList xs t) = do
  xs' <- mapM (traverseTExpr f) xs
  f $ Types.TList xs' t
traverseTExpr f (TArray xs t) = do
  xs' <- mapM (traverseTExpr f) xs
  f $ TArray xs' t

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
  _lhs_ :: TypeExpr,
  _rhs_ :: TypeExpr
  } deriving (Ord, Eq)

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
  replaceTypeVar from to (FuncLetPattern t f xs) = (FuncLetPattern (replaceTypeVar from to t) f (fmap (replaceTypeVar from to) <$> xs))
  replaceTypeVar from to (LetPattern t s) = (LetPattern (replaceTypeVar from to t) (replaceTypeVar from to s))

instance TypeVarReplaceable TExpr where
  replaceTypeVar from@(TV fs) to orig = runIdentity $ (traverseTExpr $ pure . impl) orig
    where
      replaceInPatterns (TMatch a xs b) = TMatch a (fmap (\(p, e)-> (replaceTypeVar from to p, e)) xs) b
      replaceInPatterns (TLet p a b) = TLet (replaceTypeVar from to p) a b
      replaceInPatterns (TLetIn p a b c) = TLetIn (replaceTypeVar from to p) a b c
      replaceInPatterns (TLetRec p a b) = TLetRec (replaceTypeVar from to p) a b
      replaceInPatterns (TLetRecIn p a b c) = TLetRecIn (replaceTypeVar from to p) a b c
      replaceInPatterns x = x
      impl :: TExpr -> TExpr
      impl e = replaceInPatterns $ e & typeExpr_ .~ (replaceTypeVar from to (e ^. typeExpr_))

toTV :: TypeExpr -> Either CompileError TV
toTV (TypeVar s) = pure $ TV s
toTV v = Left $ TypeError $ pack $ show v <> "is not TypeVar"

fromTV :: TV -> TypeExpr
fromTV (TV s) = TypeVar s

L.makeLenses ''TypeConstraint
