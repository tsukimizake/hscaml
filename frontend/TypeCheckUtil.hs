{-# OPTIONS -Wall #-}
module TypeCheckUtil where
import Types

-- Exprに対するmap
mapMExpr :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
mapMExpr f x@(Constant _) = f x
mapMExpr f x@(Var _) = f x
mapMExpr f (Paren e) = do
    e' <- f e
    pure $ Paren e'
mapMExpr f (InfixOpExpr e iop g) = do
    e' <- f e
    g' <- f g
    pure $ InfixOpExpr e' iop g'
mapMExpr f (BegEnd e) = do
    e' <- f e
    pure $ BegEnd e'
mapMExpr f (MultiExpr e) = do
    e' <- mapM f e
    pure $ MultiExpr e'
mapMExpr f (Constr e) = do
    e' <- f e
    pure $ Constr e'
mapMExpr f (IfThenElse e1 e2 e3) = do
    e1' <- f e1
    e2' <- f e2
    e3' <- f e3
    pure $ IfThenElse e1' e2' e3'
mapMExpr f (Match e1 e2) = do
    e1' <- f e1
    e2' <- mapM (\(x, y) -> do
                     y' <- f y
                     pure (x, y')) e2
    pure $ Match e1' e2'
mapMExpr f (While e1 e2) = do
    e1' <- f e1
    e2' <- f e2
    pure $ While e1' e2'
mapMExpr f (FunApply e1 e2) = do
    e1' <- f e1
    e2' <- mapM f e2
    pure $ FunApply e1' e2'
mapMExpr f (Let e1 e2) =  do
    e2' <- f e2
    pure $ Let e1 e2'
mapMExpr f (LetRec e1 e2) =  do
    e2' <- f e2
    pure $ LetRec e1 e2'
mapMExpr f (LetIn e1 e2 e3) = do
    e2' <- f e2
    e3' <- f e3
    pure $ LetIn e1 e2' e3'
mapMExpr _ (TypeDecl e1 e2) = pure $ TypeDecl e1 e2

mapMTExpr :: (Monad m) => (TExpr -> m TExpr) -> TExpr -> m TExpr
mapMTExpr f x@(TConstant _ _) = f x
mapMTExpr f x@(TVar _ _) = f x
mapMTExpr f (TParen e t) = do
    e' <- f e
    f $ TParen e' t
mapMTExpr f (TInfixOpExpr e iop g t) = do
    e' <- f e
    g' <- f g
    pure $ TInfixOpExpr e' iop g' t
mapMTExpr f (TBegEnd e t) = do
    e' <- f e
    f $ TBegEnd e' t
mapMTExpr f (TMultiExpr e t) = do
    e' <- mapM f e
    f $ TMultiExpr e' t
mapMTExpr f (TConstr e t) = do
    e' <- f e
    f $ TConstr e' t
mapMTExpr f (TIfThenElse e1 e2 e3 t) = do
    e1' <- f e1
    e2' <- f e2
    e3' <- f e3
    f $ TIfThenElse e1' e2' e3' t
mapMTExpr f (TMatch e1 e2 t) = do
    e1' <- f e1
    e2' <- mapM (\(x, y) -> do
                     y' <- f y
                     pure (x, y')) e2
    f $ TMatch e1' e2' t
mapMTExpr f (TWhile e1 e2 t) = do
    e1' <- f e1
    e2' <- f e2
    f $ TWhile e1' e2' t
mapMTExpr f (TFunApply e1 e2 t) = do
    e2' <- mapM f e2
    f $ TFunApply e1 e2' t
mapMTExpr f (TLet e1 e2 t) =  do
    e2' <- f e2
    f $ TLet e1 e2' t
mapMTExpr f (TLetRec e1 e2 t) =  do
    e2' <- f e2
    f $ TLetRec e1 e2' t
mapMTExpr f (TLetIn e1 e2 e3 t) = do
    e2' <- f e2
    e3' <- f e3
    f $ TLetIn e1 e2' e3' t
mapMTExpr f (TTypeDecl e1 e2 t) = do
  f $ TTypeDecl e1 e2 t
