{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module TypeCheckUtil(renameSymsByScope, mapMExpr) where
import Control.Monad.State.Strict
import Data.Map as M
import Control.Lens
import Types
import qualified Data.Text as T hiding (head)
import Data.Maybe
import Data.Monoid

data RenameState =
    RenameState {
        _counter :: Map Name Int,
        _renameStack :: Map Name [Name]
    } deriving (Show)

makeLenses ''RenameState


initialRenameState :: RenameState
initialRenameState = RenameState {_counter=M.empty, _renameStack=M.empty}

unwrapSym :: Sym -> Name
unwrapSym s = s ^. Types.name

pushAndRenameSym :: Name -> State RenameState Name
pushAndRenameSym s = do
        s' <- genSym s
        stack <- use renameStack
        let xs = fromMaybe [] (stack ^. at s) :: [Name]
        renameStack .= (stack & at s ?~ (s':xs))
        pure s'

popRenameStack :: Name -> State RenameState ()
popRenameStack s = do
    stack <- use renameStack
    let xs = fromJust (stack ^. at s)
    renameStack .= (stack & at s ?~ tail xs)
    pure ()

genSym :: Name -> State RenameState Name
genSym x = do
    oldmap <- use counter
    let n = fromMaybe 0 (oldmap ^. at x) :: Int
    let newmap = oldmap & at x ?~ (n+1)
    counter .= newmap
    pure $ "_" <> x <> "_gen_" <> (T.pack $ show n)

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
    e2' <- mapM f e2
    pure $ FunApply e1 e2'
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

-- let x = 0
-- in let x = 1
-- in x
-- =>
-- let x0 = 0
-- in let x1 = 1
-- in x1
renameSymsByScope :: Expr -> Expr
renameSymsByScope e = evalState (impl e) initialRenameState
  where
    impl :: Expr -> State RenameState Expr
    impl (V s) = do
        stack <- use renameStack
        let newname = stack ^. at s & fromJust & head
        pure $ V newname
    impl x@(IntC _) = pure $ x
    impl (Let (VarPattern t (Sym s)) e) = do
        s' <- pushAndRenameSym s
        e' <- impl e
        popRenameStack s
        pure (Let (VarPattern t (Sym s')) e')
    impl (LetRec (VarPattern t (Sym s)) e) = do
        s' <- pushAndRenameSym s
        e' <- impl e
        popRenameStack s
        pure (LetRec (VarPattern t (Sym s')) e')
    impl (LetIn (VarPattern t (Sym s)) e1 e2) = do
        s' <- pushAndRenameSym s
        e1' <- impl e1
        e2' <- impl e2
        popRenameStack s
        pure (LetIn (VarPattern t (Sym s')) e1' e2')

    impl (Let (FuncPattern t (Sym s) xs) e) = do
        s' <- pushAndRenameSym s
        argsText' <- mapM (pushAndRenameSym . unwrapSym) xs
        let args' = fmap Sym argsText'
        e' <- impl e
        popRenameStack s
        mapM_ (popRenameStack . unwrapSym) xs
        pure $ Let (FuncPattern t (Sym s') (args')) e'
    impl (LetRec (FuncPattern t (Sym s) xs) e) = do
        s' <- pushAndRenameSym s
        argsText' <- mapM (pushAndRenameSym . unwrapSym) xs
        let args' = fmap Sym argsText'
        e' <- impl e
        popRenameStack s
        mapM_ (popRenameStack . unwrapSym) xs
        pure $ LetRec (FuncPattern t (Sym s') (args')) e'

    impl (LetIn (FuncPattern t (Sym s) xs) e1 e2) = do
        s' <- pushAndRenameSym s
        argsText' <- mapM (pushAndRenameSym . unwrapSym) xs
        let args' = fmap Sym argsText'
        e1' <- impl e1
        e2' <- impl e2
        popRenameStack s
        mapM_ (popRenameStack . unwrapSym) xs
        pure $ LetIn (FuncPattern t (Sym s') (args')) e1' e2'
    impl e = mapMExpr impl e
