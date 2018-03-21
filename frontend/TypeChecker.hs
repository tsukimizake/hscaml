{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module TypeChecker where

import Control.Zipper
import Types
import OCamlType
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.State
import Data.Text
import Data.Monoid
import TypeCheckUtil
import Data.Map as M
import Data.Maybe
import Data.Set as S
import Debug.Trace
import Parser

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
L.makeLenses ''MangleTypeVarStat

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
      vars <- L.use _varName
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
              n <- L.use _genSymNumber
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
  e' <- initialTypeInfer e
  f' <- mapM initialTypeInfer f
  t <- genTypeVar Nothing Nothing
  pure $ TFunApply e' f' t
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
  TypeOfExpr{
  __texpr :: TExpr
  } deriving (Show, Ord, Eq)

data TIStep =
  TIStep{
  __Expr :: TExpr,
  __constraints :: [TypeConstraint]
        } deriving(Show, Ord, Eq)

L.makeLenses ''TypeConstraint

collectTypeConstraintsStmt :: TStatement -> CollectTypeConstraintsM (S.Set TypeConstraint)
collectTypeConstraintsStmt (TStatement exprs) = do
  forM_ exprs $ \expr -> do
    collectTypeConstraints expr
  get

type CollectTypeConstraintsM a = State (S.Set TypeConstraint) a

putTypeConstraint :: TypeConstraint -> CollectTypeConstraintsM ()
putTypeConstraint tc = do
  modify' (S.insert tc)

initialCollectTypeConstaraintsState = S.empty

collectTypeConstraints :: TExpr -> CollectTypeConstraintsM ()
collectTypeConstraints exp@(TLetIn pat impl body t) = do
  _ <- collectFromPattern pat (impl ^. _typeExpr)
  putTypeConstraint $ TypeEq (body ^. _typeExpr) t
  putTypeConstraint $ TypeOfExpr exp
  collectTypeConstraints impl
  collectTypeConstraints body
    where collectFromPattern :: Pattern -> TypeExpr -> CollectTypeConstraintsM (Maybe TypeConstraint) -- パターンがVarPattern/FuncPatternのときはそのシンボルの型を返す
          -- rtypeはパターンマッチの右辺の型。フォースが共にあらんことを。
          collectFromPattern (VarPattern thetype sym) rtype = do
            let c = TypeOfExpr (TVar sym thetype)
            putTypeConstraint $ c
            pure $ Just c
          collectFromPattern (FuncPattern t f args) rtype = do
            let c = TypeOfExpr (TVar f t)
            putTypeConstraint c

            let functype = buildFuncType rtype (fmap snd args)
            putTypeConstraint $ TypeEq functype t

            forM_ args $ \(s, t) -> do
              putTypeConstraint $ TypeOfExpr (TVar s t)
            pure $ Just c
              where
                buildFuncType :: TypeExpr -> [TypeExpr] -> TypeExpr
                buildFuncType ret [] = ret
                buildFuncType ret (x:xs) = x ::-> buildFuncType ret xs
          collectFromPattern (ParenPattern theType pat) rtype = do
            c <- collectFromPattern pat rtype
            case c of
              Just (TypeOfExpr (TVar s t)) -> do -- ? TVarでないTExprなら?
                putTypeConstraint $ TypeEq t theType
                pure Nothing
              _ ->
                pure Nothing
          collectFromPattern (ConstantPattern _ _) _ = pure Nothing
          collectFromPattern (ListPattern _ _) _ = pure Nothing
          collectFromPattern (OrPattern _ _ _) _ = pure Nothing

collectTypeConstraints (TFunApply func args t) = do
  let constraintType = makeConstraintType func args t
  putTypeConstraint $ TypeEq constraintType (func ^. _typeExpr)
  putTypeConstraint $ TypeOfExpr func
  mapM_ (putTypeConstraint . TypeOfExpr) args
  collectTypeConstraints func
  mapM_ collectTypeConstraints args
    where
      makeConstraintType :: TExpr -> [TExpr] -> TypeExpr -> TypeExpr
      makeConstraintType _ [] t = t
      makeConstraintType s (x:xs) t = (x ^. _typeExpr) ::-> makeConstraintType s xs t
collectTypeConstraints (TParen inner outtype) = do
  putTypeConstraint $ TypeEq (inner ^. _typeExpr) outtype
  collectTypeConstraints inner

collectTypeConstraints (TInfixOpExpr l op r t)
  | elem op [Plus, Minus, Mul, Div, Mod] = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) ocamlInt
      putTypeConstraint $ TypeEq (r ^. _typeExpr) ocamlInt
      putTypeConstraint $ TypeEq t ocamlInt
  | elem op [PlusDot, MinusDot, MulDot, DivDot] = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) ocamlFloat
      putTypeConstraint $ TypeEq (r ^. _typeExpr) ocamlFloat
      putTypeConstraint $ TypeEq t ocamlFloat
  | elem op [BoolAnd, BoolOr] = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) ocamlBool
      putTypeConstraint $ TypeEq (r ^. _typeExpr) ocamlBool
      putTypeConstraint $ TypeEq t ocamlBool
  | isComp op = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) (r ^. _typeExpr)
      putTypeConstraint $ TypeEq t ocamlBool
  | otherwise = do
      error $ "oprator" <> show op <> "'s type constraint is undefined"
    where
      isComp (Compare _) = True
      isComp _ = False
collectTypeConstraints _ = pure ()

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  let e' = renameSymsByScope e
  let te' = (initialTypeInfer e') `evalState` initialMangleTypeVarStat
  let constraints = (collectTypeConstraints te') `evalState` initialCollectTypeConstaraintsState
  pure te'

traceTexpr s = do
    let expr = renameSymsByScope . parseExpr $ s
    let texpr = (initialTypeInfer expr) `evalState` initialMangleTypeVarStat
    print texpr
    let constraints = (collectTypeConstraints texpr) `execState` initialCollectTypeConstaraintsState
    print constraints
