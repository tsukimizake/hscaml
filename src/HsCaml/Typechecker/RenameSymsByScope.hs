{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS -Wall -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module HsCaml.TypeChecker.RenameSymsByScope (renameSymsByScope) where
import HsCaml.FrontEnd.Types as Types
import Control.Monad.State.Strict
import Data.Maybe
import Control.Lens
import HsCaml.TypeChecker.TypeCheckUtil
import qualified HsCaml.Common.Gensym as GS
import qualified Data.Extensible as E
import qualified Data.Extensible.Effect.Default as E
import Data.Proxy
import qualified Data.Text as T

type RenameSymsEff = E.Eff '["State" E.>: GS.GensymM, "err" E.>: E.EitherEff CompileError]

genSym :: T.Text -> RenameSymsEff T.Text
genSym s = E.liftEff (Proxy :: Proxy "State") (GS.genSym s)

pushAndRenameSym :: Name -> RenameSymsEff Name
pushAndRenameSym s = do
        s' <- genSym s
        stack <- use GS.renameStack
        let xs = fromMaybe [] (stack ^. at s) :: [Name]
        GS.renameStack .= (stack & at s ?~ (s':xs))
        pure s'

popRenameStack :: Name -> RenameSymsEff ()
popRenameStack s = do
    stack <- use GS.renameStack
    let xs = stack ^. at s
    case xs of
      Nothing -> E.throwEff (Proxy :: Proxy "err") $ SemanticsError ""
      Just xs -> GS.renameStack .= (stack & at s ?~ Prelude.tail xs)

unwrapSym :: Sym -> Name
unwrapSym s = s ^. _name

-- let x = 0
-- in let x = 1
-- in x
-- =>
-- let x0 = 0
-- in let x1 = 1
-- in x1
-- TODO fromJust : Nothingとかじゃなくエラー処理をですね…

renameSymsByScope :: Expr -> Either CompileError Expr
renameSymsByScope expr = E.leaveEff . E.runEitherEff . flip E.evalStateEff GS.initialGensymState $ impl expr
  where
    impl :: Expr -> RenameSymsEff Expr
    impl (Var (Sym s)) = do
        stack <- use GS.renameStack
        let got = stack ^. at s -- & fromJust & head
        case got of
          Nothing -> E.throwEff (Proxy :: Proxy "err") . SemanticsError $ T.intercalate " " ["Symbol", s, "couldn't be found"]
          Just xs ->
            case length xs of
              0 -> E.throwEff (Proxy :: Proxy "err") . SemanticsError $ T.intercalate " " ["Symbol", s, "couldn't be found"]
              _ -> pure $ V (head xs)
    impl x@(IntC _) = pure $ x
    impl (Let (LetPatternPattern t1 (VarPattern t (Sym s))) e) = do
        e' <- impl e
        s' <- pushAndRenameSym s
        popRenameStack s
        pure (Let (LetPatternPattern t1 (VarPattern t (Sym s'))) e')
    impl (LetRec (LetPatternPattern t1 (VarPattern t (Sym s))) e) = do
        s' <- pushAndRenameSym s
        e' <- impl e
        popRenameStack s
        pure (LetRec (LetPatternPattern t1 (VarPattern t (Sym s'))) e')
    impl (LetIn (LetPatternPattern t1 (VarPattern t (Sym s))) e1 e2) = do
        e1' <- impl e1
        s' <- pushAndRenameSym s
        e2' <- impl e2
        popRenameStack s
        pure (LetIn (LetPatternPattern t1 (VarPattern t (Sym s'))) e1' e2')
    impl (Let (FuncLetPattern t (Sym s) xs) e) = do
        e' <- impl e
        s' <- pushAndRenameSym s
        args' <- forM xs $ \(s, t) -> do
          s' <- pushAndRenameSym $ unwrapSym $ s
          pure (Sym s', t)
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
        args' <- forM xs $ \(s, t) -> do
              s' <- pushAndRenameSym $ unwrapSym $ s
              pure (Sym s', t)
        e1' <- impl e1
        s' <- pushAndRenameSym s
        e2' <- impl e2
        popRenameStack s
        forM_ xs $ \(s, t) -> do
          s' <- popRenameStack . unwrapSym $ s
          pure (s', t)
        pure $ LetIn (FuncLetPattern t (Sym s') (args')) e1' e2'
    impl (LetRecIn (LetPatternPattern t1 (VarPattern t (Sym s))) e1 e2) = do
        s' <- pushAndRenameSym s
        e1' <- impl e1
        e2' <- impl e2
        popRenameStack s
        pure (LetRecIn (LetPatternPattern t1 (VarPattern t (Sym s'))) e1' e2')

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
    impl e@(Constant _) = pure e
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
    impl e@(Types.List _) = traverseExpr impl e
    impl e@(Array _) = traverseExpr impl e
