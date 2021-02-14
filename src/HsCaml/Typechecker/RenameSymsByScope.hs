{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# OPTIONS -Wno-name-shadowing #-}
module HsCaml.TypeChecker.RenameSymsByScope (renameSymsByScope) where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Extensible as E
import qualified Data.Extensible.Effect as E
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import qualified HsCaml.Common.Gensym as GS
import HsCaml.FrontEnd.Types as Types
import HsCaml.TypeChecker.TypeCheckUtil

type RenameSymsEff = E.Eff '["State" E.>: GS.GensymM, "err" E.>: E.EitherEff CompileError]

genSym :: T.Text -> RenameSymsEff T.Text
genSym s = E.liftEff (Proxy @"State") (GS.genSym s)

liftState :: GS.GensymM a -> RenameSymsEff a
liftState = E.liftEff (Proxy @"State")

pushAndRenameSym :: Sym -> RenameSymsEff Sym
pushAndRenameSym s = do
  s' <- genSym s
  stack <- liftState $ use GS.renameStack
  let xs = fromMaybe [] (stack ^. at s) :: [Sym]
  liftState $ GS.renameStack .= (stack & at s ?~ (s' : xs))

  pure s'

popRenameStack :: Sym -> RenameSymsEff ()
popRenameStack s = do
  stack <- liftState $ use GS.renameStack
  let xs = stack ^. at s
  case xs of
    Nothing -> E.throwEff (Proxy @"err") $ SemanticsError ""
    Just xs -> liftState $ GS.renameStack .= (stack & at s ?~ Prelude.tail xs)

-- let x = 0
-- in let x = 1
-- in x
-- =>
-- let x0 = 0
-- in let x1 = 1
-- in x1

renameSymsByScope :: GS.GensymState -> Expr -> Either CompileError Expr
renameSymsByScope initialState expr = E.leaveEff . E.runEitherEff . flip E.evalStateEff initialState $ impl expr
  where
    impl :: Expr -> RenameSymsEff Expr
    impl (Var s _) = do
      stack <- liftState $ use GS.renameStack
      let got = stack ^. at s
      case got of
        Nothing -> E.throwEff (Proxy @"err") . SemanticsError $ T.intercalate " " ["Symbol", s, "not found"]
        Just xs ->
          case length xs of
            0 -> E.throwEff (Proxy @"err") . SemanticsError $ T.intercalate " " ["Symbol", s, "not found"]
            _ -> pure $ V (head xs)
    impl x@(IntC _) = pure x
    impl (Let (LetPattern t1 (VarPattern t s)) e t2) = do
      e' <- impl e
      s' <- pushAndRenameSym s
      popRenameStack s
      pure (Let (LetPattern t1 (VarPattern t s')) e' t2)
    impl (LetRec (LetPattern t1 (VarPattern t s)) e t2) = do
      s' <- pushAndRenameSym s
      e' <- impl e
      popRenameStack s
      pure (LetRec (LetPattern t1 (VarPattern t s')) e' t2)
    impl (LetIn (LetPattern t1 (VarPattern t s)) e1 e2 t2) = do
      e1' <- impl e1
      s' <- pushAndRenameSym s
      e2' <- impl e2
      popRenameStack s
      pure (LetIn (LetPattern t1 (VarPattern t s')) e1' e2' t2)
    impl (Let (FuncLetPattern t s xs) e t2) = do
      s' <- pushAndRenameSym s
      args' <- forM xs $ \(s, t) -> do
        s' <- pushAndRenameSym s
        pure (s', t)
      e' <- impl e
      popRenameStack s
      forM_ xs $ \(s, t) -> do
        s' <- popRenameStack s
        pure (s', t)
      pure $ Let (FuncLetPattern t s' args') e' t2
    impl (LetRec (FuncLetPattern t s xs) e t2) = do
      s' <- pushAndRenameSym s
      args' <- forM xs $ \(s, t) -> do
        s' <- pushAndRenameSym s
        pure (s', t)
      e' <- impl e
      popRenameStack s
      forM_ xs $ \(s, t) -> do
        s' <- popRenameStack s
        pure (s', t)
      pure $ LetRec (FuncLetPattern t s' args') e' t2
    impl (LetIn (FuncLetPattern t s xs) e1 e2 t2) = do
      args' <- forM xs $ \(s, t) -> do
        s' <- pushAndRenameSym s
        pure (s', t)
      e1' <- impl e1
      s' <- pushAndRenameSym s
      e2' <- impl e2
      popRenameStack s
      forM_ xs $ \(s, t) -> do
        s' <- popRenameStack s
        pure (s', t)
      pure $ LetIn (FuncLetPattern t s' args') e1' e2' t2
    impl (LetRecIn (LetPattern t1 (VarPattern t s)) e1 e2 t2) = do
      s' <- pushAndRenameSym s
      e1' <- impl e1
      e2' <- impl e2
      popRenameStack s
      pure (LetRecIn (LetPattern t1 (VarPattern t s')) e1' e2' t2)
    impl (LetRecIn (FuncLetPattern t s xs) e1 e2 t2) = do
      s' <- pushAndRenameSym s
      args' <- forM xs $ \(s, t) -> do
        s' <- pushAndRenameSym s
        pure (s', t)
      e1' <- impl e1
      e2' <- impl e2
      popRenameStack s
      forM_ xs $ \(s, t) -> do
        s' <- popRenameStack s
        pure (s', t)
      pure $ LetRecIn (FuncLetPattern t s' args') e1' e2' t2
    impl (FunApply (V f) args t) = do
      f' <- impl $ V f
      args' <- forM args $ \arg -> do
        impl arg
      pure $ FunApply f' args' t
    impl e@(Constant _ _) = pure e
    impl e@(Paren _ _) = traverseExpr impl e
    impl e@(InfixOpExpr _ _ _ _) = traverseExpr impl e
    impl e@(BegEnd _ _) = traverseExpr impl e
    impl e@(MultiExpr _ _) = traverseExpr impl e
    impl e@(Constr _ _) = traverseExpr impl e
    impl e@(Match _ _ _) = traverseExpr impl e
    impl e@(While _ _ _) = traverseExpr impl e
    impl e@(IfThenElse _ _ _ _) = traverseExpr impl e
    impl e@(FunApply _ _ _) = traverseExpr impl e
    impl e@(Let (LetPattern _ _) _ _) = traverseExpr impl e
    impl e@(LetIn (LetPattern _ _) _ _ _) = traverseExpr impl e
    impl e@(LetRec (LetPattern _ _) _ _) = traverseExpr impl e
    impl e@(LetRecIn (LetPattern _ _) _ _ _) = traverseExpr impl e
    impl e@(Types.List _ _) = traverseExpr impl e
    impl e@(Array _ _) = traverseExpr impl e
