{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HsCaml.TypeChecker.InitialTypeInfer (initialTypeInfer) where

import HsCaml.FrontEnd.Types
import HsCaml.FrontEnd.OCamlType
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.State
import Data.Text
import Data.Monoid
import HsCaml.TypeChecker.TypeCheckUtil
import Data.Map as M
import Data.Maybe
import Data.Set as S
import Debug.Trace
import TextShow

type SymName = Text
type TypeName = Text
data MangleTypeVarStat = MangleTypeVarStat{
  _genSymNumber_ :: Int,
  _varName_ :: Map SymName TypeName
  }deriving(Show)

initialMangleTypeVarStat :: MangleTypeVarStat
initialMangleTypeVarStat = MangleTypeVarStat 0 M.empty

type MangleTypeVarM a = State MangleTypeVarStat a
L.makeLenses ''MangleTypeVarStat

-- 違うTypeExpr内の同じTypeVarは違う型であってほしいのでそこを別の名前にする。
-- 下の[全ての型式のtypevarをリネームする前処理が必要(TODO)]がこれの予定
renameTypeVarByTypeExpr :: MangleTypeVarM Expr
renameTypeVarByTypeExpr = undefined

-- もう見たsymなら同じTypeVarに、初めて見たsymなら新しいtypevarをつける(symはrenameTypeVariablesで同じ名なら同じブツだと保証済 (Value Constraintsは知らん))
-- TODO let f (x:'a) (y:'a) = x*y みたいのどうすんだ？ 同じ型式の中でだけ同じtypevarは同じ。ということで、全ての型式のtypevarをリネームする前処理が必要(TODO)
genTypeVar ::Maybe SymName -> Maybe TypeName -> MangleTypeVarM TypeExpr
genTypeVar symm Nothing = genTypeVar symm (Just "")
genTypeVar symm (Just tname) =
  case symm of
    Nothing -> do
      typename <- genNewTypeName tname
      pure $ TypeVar typename
    Just sym -> do
      vars <- L.use varName_
      let lookedup = M.lookup sym vars
      if (isJust lookedup)
        then do
        let lookedup' = fromJust lookedup
        pure $ TypeVar lookedup'
        else do
        typename <- genNewTypeName tname
        varName_ %= M.insert sym typename
        pure $ TypeVar typename
genNewTypeName :: MonadState MangleTypeVarStat m => Text -> m Text
genNewTypeName defaultName = do
  n <- L.use genSymNumber_
  genSymNumber_ .= (n+1)
  pure $ "_" <> defaultName <> showt n

symToText :: Sym -> Text
symToText (Sym x) = x

-- とりあえずプリミティブや即値やアノテーション書かれたやつにだけ型を付ける, あとtypevarのリネーム
initialTypeInferImpl :: Expr -> MangleTypeVarM TExpr
initialTypeInferImpl (Constant x@(IntVal _)) = pure $ TConstant x ocamlInt
initialTypeInferImpl (Constant x@(BoolVal _)) = pure $ TConstant x ocamlBool
initialTypeInferImpl (Var x) = do
  t <- genTypeVar (Just $ symToText x) Nothing
  pure $ TVar x t
initialTypeInferImpl (Paren e) = do
  e' <- initialTypeInferImpl e
  t <- genTypeVar Nothing Nothing
  pure $ TParen e' t
initialTypeInferImpl (InfixOpExpr e f g) = do
  e' <- initialTypeInferImpl e
  g' <- initialTypeInferImpl g
  t <- genTypeVar Nothing Nothing
  pure $ TInfixOpExpr e' f g' t
initialTypeInferImpl (BegEnd e) = do
  e' <- initialTypeInferImpl e
  t <- genTypeVar Nothing Nothing
  pure $ TBegEnd e' t
initialTypeInferImpl (MultiExpr e) = do
  e' <- mapM initialTypeInferImpl e
  t <- genTypeVar Nothing Nothing
  pure $ TMultiExpr e' t
initialTypeInferImpl (Constr e) = do
  e' <- initialTypeInferImpl e
  t <- genTypeVar Nothing Nothing
  pure $ TConstr e' t
initialTypeInferImpl (IfThenElse e f g) = do
  e' <- initialTypeInferImpl e
  f' <- initialTypeInferImpl f
  g' <- initialTypeInferImpl g
  t <- genTypeVar Nothing Nothing
  pure $ TIfThenElse e' f' g' t
initialTypeInferImpl (Match e f) = do
  e' <- initialTypeInferImpl e
  f' <- mapM (mapM initialTypeInferImpl) f :: MangleTypeVarM [(Pattern, TExpr)]
  t <- genTypeVar Nothing Nothing
  pure $ TMatch e' f' t
initialTypeInferImpl (While e f) = do
  e' <- initialTypeInferImpl e
  f' <- initialTypeInferImpl f
  t <- genTypeVar Nothing Nothing
  pure $ TWhile e' f' t
initialTypeInferImpl (FunApply e f) = do
  e' <- initialTypeInferImpl e
  f' <- mapM initialTypeInferImpl f
  t <- genTypeVar Nothing Nothing
  pure $ TFunApply e' f' t
initialTypeInferImpl (Let pat f) = do
  pat' <- nameTypeVarInLetPat pat
  f' <- initialTypeInferImpl f
  pure $ TLet pat' f' ocamlUnit
initialTypeInferImpl (LetRec pat f) = do
  pat' <- nameTypeVarInLetPat pat
  f' <- initialTypeInferImpl f
  pure $ TLetRec pat' f' ocamlUnit
initialTypeInferImpl (LetIn pat f g) = do
  pat' <- nameTypeVarInLetPat pat
  f' <- initialTypeInferImpl f
  g' <- initialTypeInferImpl g
  t <- genTypeVar Nothing Nothing
  pure $ TLetIn pat' f' g' t
initialTypeInferImpl (LetRecIn pat f g) = do
  pat' <- nameTypeVarInLetPat pat
  f' <- initialTypeInferImpl f
  g' <- initialTypeInferImpl g
  t <- genTypeVar Nothing Nothing
  pure $ TLetRecIn pat' f' g' t
initialTypeInferImpl (List xs) = do
  t <- genTypeVar Nothing Nothing
  ys <- mapM initialTypeInferImpl xs :: MangleTypeVarM [TExpr]
  pure $ TList ys t
initialTypeInferImpl (Array xs) = do
  t <- genTypeVar Nothing Nothing
  ys <- mapM initialTypeInferImpl xs :: MangleTypeVarM [TExpr]
  pure $ TArray ys t

initialTypeInfer :: Expr -> TExpr
initialTypeInfer expr = (initialTypeInferImpl expr) `evalState` initialMangleTypeVarStat

initialTypeInferStmt :: Statement -> MangleTypeVarM TStatement
initialTypeInferStmt (Statement toplevels) = do
  tes <- mapM impltexpr toplevels
  tds <- mapM impltypedecls toplevels
  pure $ TStatement (catMaybes tes) (catMaybes tds)
  where
    impltexpr (TopLevelExpr expr) = do
      texpr <- initialTypeInferImpl expr
      pure $ Just $ texpr
    impltexpr (TopLevelTypeDecl _) = pure Nothing
    impltypedecls (TopLevelTypeDecl td) = pure $ Just $ td
    impltypedecls (TopLevelExpr _) = pure Nothing

nameTypeVarInLetPat :: LetPattern -> MangleTypeVarM LetPattern
nameTypeVarInLetPat (LetPatternPattern t1 (VarPattern t s)) = do
  t' <- genTypeVar (Just $ symToText s) Nothing
  pure $ LetPatternPattern t' (VarPattern t' s)
nameTypeVarInLetPat e@(LetPatternPattern _ _) =
  pure e
nameTypeVarInLetPat (FuncLetPattern t f xs) = do
  t' <- genTypeVar (Just $ symToText f) Nothing
  xs' <- forM xs $ \(s, t) -> do
    (LetPatternPattern _ (VarPattern t' _)) <- nameTypeVarInLetPat $ LetPatternPattern UnspecifiedType (VarPattern t s)
    pure (s, t')
  pure $ FuncLetPattern t' f xs'
