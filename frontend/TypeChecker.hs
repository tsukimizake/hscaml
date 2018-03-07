{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module TypeChecker where

import Control.Zipper
import Types
import OCamlType
import Control.Lens
import Control.Monad.State
import Data.Text
import Data.Monoid
import TypeCheckUtil
import Data.Map as M
import Data.Maybe
import Data.Set as S

textShow :: (Show a) => a -> Text
textShow = pack . show

type SymName = Text
type TypeName = Text
data MangleTypeVarStat = MangleTypeVarStat{
  __genSymNumber :: Int,
  __varName :: Map SymName TypeName
  }deriving(Show)

initialMangleTypeVarStat = MangleTypeVarStat 0 M.empty

type MangleTypeVarM a = State MangleTypeVarStat a
makeLenses ''MangleTypeVarStat

-- 違うTypeExpr内の同じTypeVarは違う型であってほしいのでそこを別の名前にする。
-- 下の[全ての型式のtypevarをリネームする前処理が必要(TODO)]がこれの予定
renameTypeVarByTypeExpr :: MangleTypeVarM Expr
renameTypeVarByTypeExpr = undefined

-- もう見たsymなら同じTypeVarに、初めて見たsymなら新しいtypevarをつける(symはrenameTypeVariablesで同じ名なら同じブツだと保証済 (Value Constraintsは知らん))
-- TODO let f (x:'a) (y:'a) = x*y みたいのどうすんだ？ 同じ型式の中でだけ同じtypevarは同じ。ということで、全ての型式のtypevarをリネームする前処理が必要(TODO)
genTypeVar ::Maybe SymName -> Maybe TypeName -> MangleTypeVarM TypeExpr
genTypeVar symm Nothing = genTypeVar symm (Just "")
genTypeVar symm (Just tname)
  | symm == Nothing = do
      typename <- genNewTypeName tname
      pure $ TypeVar typename
  | otherwise = do
      let sym = fromJust symm
      vars <- use _varName
      let lookedup = M.lookup sym vars
      if (isJust lookedup)
        then do
        let lookedup' = fromJust lookedup
        pure $ TypeVar lookedup'
        else do
        typename <- genNewTypeName tname
        _varName %= M.insert sym typename
        pure $ TypeVar typename
          where
            genNewTypeName defaultName = do
              n <- use _genSymNumber
              _genSymNumber .= (n+1)
              pure $ "_" <> defaultName <> textShow n

symToText :: Sym -> Text
symToText (Sym x) = x

-- とりあえずプリミティブや即値やアノテーション書かれたやつにだけ型を付ける, あとtypevarのリネーム
initialTypeInfer :: Expr -> MangleTypeVarM TExpr
initialTypeInfer (Constant x@(IntVal _)) = pure $ TConstant x ocamlInt
initialTypeInfer (Constant x@(BoolVal _)) = pure $ TConstant x ocamlBool
initialTypeInfer (Var x) = do
  t <- genTypeVar (Just $ symToText x) Nothing
  pure $ TVar x t
initialTypeInfer (Paren e) = do
  e' <- initialTypeInfer e
  t <- genTypeVar Nothing Nothing
  pure $ TParen e' t
initialTypeInfer (InfixOpExpr e f g) = do
  e' <- initialTypeInfer e
  g' <- initialTypeInfer g
  t <- genTypeVar Nothing Nothing
  pure $ TInfixOpExpr e' f g' t
initialTypeInfer (BegEnd e) = do
  e' <- initialTypeInfer e
  t <- genTypeVar Nothing Nothing
  pure $ TBegEnd e' t
initialTypeInfer (MultiExpr e) = do
  e' <- mapM initialTypeInfer e
  t <- genTypeVar Nothing Nothing
  pure $ TMultiExpr e' t
initialTypeInfer (Constr e) = do
  e' <- initialTypeInfer e
  t <- genTypeVar Nothing Nothing
  pure $ TConstr e' t
initialTypeInfer (IfThenElse e f g) = do
  e' <- initialTypeInfer e
  f' <- initialTypeInfer f
  g' <- initialTypeInfer g
  t <- genTypeVar Nothing Nothing
  pure $ TIfThenElse e' f' g' t
initialTypeInfer (Match e f) = do
  e' <- initialTypeInfer e
  f' <- mapM (mapM (initialTypeInfer)) f :: MangleTypeVarM [(Pattern, TExpr)]
  t <- genTypeVar Nothing Nothing
  pure $ TMatch e' f' t
initialTypeInfer (While e f) = do
  e' <- initialTypeInfer e
  f' <- initialTypeInfer f
  t <- genTypeVar Nothing Nothing
  pure $ TWhile e' f' t
initialTypeInfer (FunApply e f) = do
  f' <- mapM initialTypeInfer f
  t <- genTypeVar Nothing Nothing
  pure $ TFunApply e f' t
initialTypeInfer (Let e f) = do
  f' <- initialTypeInfer f
  t <- genTypeVar Nothing Nothing
  pure $ TLet e f' t
initialTypeInfer (LetRec e f) = do
  f' <- initialTypeInfer f
  t <- genTypeVar Nothing Nothing
  pure $ TLetRec e f' t
initialTypeInfer (LetIn pat f g) = do
  pat' <- nameTypeVarInPat pat
  f' <- initialTypeInfer f
  g' <- initialTypeInfer g
  t <- genTypeVar Nothing Nothing
  pure $ TLetIn pat' f' g' t
    where
      nameTypeVarInPat :: Pattern -> MangleTypeVarM Pattern
      nameTypeVarInPat (VarPattern t s) = do
        t' <- genTypeVar (Just $ symToText s) Nothing
        pure $ VarPattern t' s
      nameTypeVarInPat (FuncPattern t f xs) = do
        t' <- genTypeVar (Just $ symToText f) Nothing
        xs' <- forM xs $ \(s, t) -> do
          (VarPattern t' _) <- nameTypeVarInPat $ VarPattern t s
          pure (s, t')
        pure $ FuncPattern t' f xs'
initialTypeInfer (TypeDecl e f) = do
  t <- genTypeVar Nothing Nothing

  pure $ TTypeDecl e f t

initialTypeInferStmt :: Statement -> MangleTypeVarM TStatement
initialTypeInferStmt (Statement exprs)= do
  texprs <- mapM initialTypeInfer exprs
  pure $ TStatement texprs

data TypeConstraint =
  TypeEq{
  __lhs :: TypeExpr,
  __rhs :: TypeExpr
  }|
  TypeOfSym{
  __symbol :: Sym,
  __type :: TypeExpr
  } deriving (Show, Ord, Eq)

data TIStep =
  TIStep{
  __Expr :: TExpr,
  __constraints :: [TypeConstraint]
        } deriving(Show, Ord, Eq)

makeLenses ''TypeConstraint

collectTypeConstraintsStmt :: TStatement -> CollectTypeConstraintsM [TypeConstraint]
collectTypeConstraintsStmt (TStatement exprs) = impl exprs
  where
    impl :: [TExpr] -> CollectTypeConstraintsM [TypeConstraint]
    impl [] = pure []
    impl (x:xs) = do
      y <- collectTypeConstraints x
      ys <- impl xs
      return $ y ++ ys

type CollectTypeConstraintsM a = State [TypeConstraint] a

putTypeConstraint :: TypeConstraint -> CollectTypeConstraintsM ()
putTypeConstraint tc = do
  modify' ((:) tc)

collectTypeConstraints :: TExpr -> CollectTypeConstraintsM [TypeConstraint]
collectTypeConstraints (TLetIn pat impl body t) = do
  collectFromPattern pat
  get
    where collectFromPattern :: Pattern -> CollectTypeConstraintsM (Maybe TypeConstraint) -- パターンがVarPattern/FuncPatternのとき、そのシンボルの型を返す
          collectFromPattern (VarPattern thetype sym) = do
            let c = TypeOfSym sym thetype
            putTypeConstraint $ c
            pure $ Just c
          collectFromPattern (FuncPattern t f args) = do
            let c = TypeOfSym f t
            putTypeConstraint $ c
            forM_ args $ \(s, t) -> do
              putTypeConstraint $ TypeOfSym s t
            pure $ Just c
          collectFromPattern (ParenPattern theType pat) = do
            c <- collectFromPattern pat
            case c of
              Just (TypeOfSym s t) -> do
                putTypeConstraint $ TypeEq t theType
                pure Nothing
              Nothing ->
                pure Nothing



collectTypeConstraints _ = pure []


typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  let e' = renameSymsByScope e
  pure $ (initialTypeInfer e') `evalState` initialMangleTypeVarStat
