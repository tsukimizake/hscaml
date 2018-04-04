{-# OPTIONS -Wall -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RenameSymsByScope (renameSymsByScope) where
import Types
import Control.Monad.State.Strict
import Data.Map as M
import Control.Lens
import qualified Data.Text as T hiding (head)
import Data.Maybe
import Data.Monoid
import TypeCheckUtil

data RenameState =
    RenameState {
        _counter :: Map Name Int,
        _renameStack :: Map Name [Name]
    } deriving (Show)

makeLenses ''RenameState

initialRenameState :: RenameState
initialRenameState = RenameState {_counter=M.empty, _renameStack=M.empty}

unwrapSym :: Sym -> Name
unwrapSym s = s ^. _name

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

-- let x = 0
-- in let x = 1
-- in x
-- =>
-- let x0 = 0
-- in let x1 = 1
-- in x1
-- TODO letがletrecと同じになってるのを修正する したい
renameSymsByScope :: Expr -> Expr
renameSymsByScope expr = evalState (impl expr) initialRenameState
  where
    impl :: Expr -> State RenameState Expr
    impl (Var (Sym s)) = do
        stack <- use renameStack
        let newname = stack ^. at s & fromJust & head
        pure $ V newname
    impl x@(IntC _) = pure $ x
    impl (Let (LetPatternPattern t1 (VarPattern t (Sym s))) e) = do
        s' <- pushAndRenameSym s
        e' <- impl e
        popRenameStack s
        pure (Let (LetPatternPattern t1 (VarPattern t (Sym s'))) e')
    impl (LetRec (LetPatternPattern t1 (VarPattern t (Sym s))) e) = do
        s' <- pushAndRenameSym s
        e' <- impl e
        popRenameStack s
        pure (LetRec (LetPatternPattern t1 (VarPattern t (Sym s'))) e')
    impl (LetIn (LetPatternPattern t1 (VarPattern t (Sym s))) e1 e2) = do
        s' <- pushAndRenameSym s
        e1' <- impl e1
        e2' <- impl e2
        popRenameStack s
        pure (LetIn (LetPatternPattern t1 (VarPattern t (Sym s'))) e1' e2')
    impl (Let (FuncLetPattern t (Sym s) xs) e) = do
        s' <- pushAndRenameSym s
        args' <- forM xs $ \(s, t) -> do
          s' <- pushAndRenameSym $ unwrapSym $ s
          pure (Sym s', t)
        e' <- impl e
        popRenameStack s
        forM_ xs $ \(s, t) -> do
          s' <- popRenameStack . unwrapSym $ s
          pure (s', t)
        pure $ Let (FuncLetPattern t (Sym s') (args')) e'
    impl (LetRec (FuncLetPattern t (Sym s) xs) e) = do
        s' <- pushAndRenameSym s
        args' <- forM xs $ \(s, t) -> do
              s' <- pushAndRenameSym $ unwrapSym $ s
              pure (Sym s', t)
        e' <- impl e
        popRenameStack s
        forM_ xs $ \(s, t) -> do
          s' <- popRenameStack . unwrapSym $ s
          pure (s', t)
        pure $ LetRec (FuncLetPattern t (Sym s') args') e'

    impl (LetIn (FuncLetPattern t (Sym s) xs) e1 e2) = do
        s' <- pushAndRenameSym s
        args' <- forM xs $ \(s, t) -> do
              s' <- pushAndRenameSym $ unwrapSym $ s
              pure (Sym s', t)
        e1' <- impl e1
        e2' <- impl e2
        popRenameStack s
        forM_ xs $ \(s, t) -> do
          s' <- popRenameStack . unwrapSym $ s
          pure (s', t)
        pure $ LetIn (FuncLetPattern t (Sym s') (args')) e1' e2'
    impl (LetRecIn (FuncLetPattern t (Sym s) xs) e1 e2) = do
        s' <- pushAndRenameSym s
        args' <- forM xs $ \(s, t) -> do
              s' <- pushAndRenameSym $ unwrapSym $ s
              pure (Sym s', t)
        e1' <- impl e1
        e2' <- impl e2
        popRenameStack s
        forM_ xs $ \(s, t) -> do
          s' <- popRenameStack . unwrapSym $ s
          pure (s', t)
        pure $ LetRecIn (FuncLetPattern t (Sym s') (args')) e1' e2'
    impl (FunApply (V f) args) = do
      f' <- impl $ V f
      args' <- forM args $ \arg -> do
        impl arg
      pure $ FunApply f' args'
    impl e@(Constant _) = traverseExpr impl e
    impl e@(Paren _) = traverseExpr impl e
    impl e@(InfixOpExpr _ _ _) = traverseExpr impl e
    impl e@(BegEnd _ ) =traverseExpr impl e
    impl e@(MultiExpr _ ) =traverseExpr impl e
    impl e@(Constr _ ) =traverseExpr impl e
    impl e@(Match _ _) =traverseExpr impl e
    impl e@(While _ _) =traverseExpr impl e
    impl e@(IfThenElse _ _ _) =traverseExpr impl e
    impl e@(FunApply _ _) =traverseExpr impl e
    impl e@(Let (LetPatternPattern _ _) _) = traverseExpr impl e
    impl e@(LetIn (LetPatternPattern _ _) _ _) = traverseExpr impl e
    impl e@(LetRec (LetPatternPattern _ _) _) = traverseExpr impl e
    impl e@(LetRecIn (LetPatternPattern _ _) _ _) = traverseExpr impl e
    impl e@(TypeDecl _ _) = traverseExpr impl e
    impl e@(Types.List _) = traverseExpr impl e
    impl e@(Array _) = traverseExpr impl e
