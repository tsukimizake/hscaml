{-# OPTIONS -Wall #-}
module TypeCheckUtil where
import Types

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
    pure $ TInfixOpExpr e' iop g' t
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
