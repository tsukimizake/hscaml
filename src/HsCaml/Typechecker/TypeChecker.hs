-- LetはまずPatternの変数のTypeOfExprだけをgmに入れる
-- bodyやってから関数の制約とかつけてunifyでうまくいくはず
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}


module HsCaml.TypeChecker.TypeChecker where

import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.InitialTypeInfer
import qualified HsCaml.TypeChecker.Unify as U
import HsCaml.TypeChecker.SetTypeConstraints
import HsCaml.TypeChecker.CheckTypeCheckIsFinished
import HsCaml.TypeChecker.TypeCheckUtil
import Control.Monad
import HsCaml.Common.Gensym
import Data.Extensible
import Data.Proxy
import Data.Foldable
import Control.Lens.Operators
import Data.List as L
import Data.Set as S
import HsCaml.FrontEnd.OCamlType
import Debug.Trace
import Data.Text (pack, Text)

type Gamma = [TypeConstraint]

type TypeCheckEff = Eff '["gs" >: GensymM, "err" >: EitherEff CompileError]

throwTypeError :: Text -> TypeCheckEff a
throwTypeError s = throwEff (Proxy::Proxy "err") $ TypeError s

-- free type variableをtvsに入れたTypeOfExprを返す
makeClosure :: Gamma -> Sym -> TypeExpr -> TypeCheckEff TypeConstraint
makeClosure gm s t = do
  let tvs = getTypeVar t L.\\ (getTypeVar =<< gm)
  pure $ TypeOfExpr tvs s t

getClosure :: Gamma -> Sym -> TypeExpr -> TypeCheckEff TypeConstraint
getClosure gm x t = do
  let rm = L.find go gm
  case rm of
    Nothing -> makeClosure gm x t
    Just r -> pure r
  where
    go  :: TypeConstraint -> Bool
    go e@(TypeOfExpr _ s _) | s == x = True
    go _ = False 

unify  :: Gamma -> TypeCheckEff Gamma
unify xs = do
  let res = U.unify xs :: Either CompileError Gamma
  -- Effわからん
  -- liftEff (Proxy::Proxy "err") res
  case res of
    Left e -> throwEff (Proxy::Proxy "err") e
    Right r -> pure r

genTypeVar :: TypeCheckEff TV 
genTypeVar = fmap TV $ liftEff (Proxy::Proxy "gs") $ genSym ""

-- Letした変数の主要型を作る 
-- gammaで言及されていない(Letで初めて作られた)型変数を[TV]に入れてTypeSchemeにする
genTypeOfExprInLet :: Gamma -> LetPattern -> TypeCheckEff Gamma
genTypeOfExprInLet gm (FuncLetPattern ft f xs) = do
  let fscm = TypeOfExpr [] f ft
  ys <- forM xs $ \(x, xt) ->
    pure $ TypeOfExpr [] x xt 
  pure $ fscm : ys 

-- パターンがVarPattern/FuncLetPatternのときはそのシンボルの型を返す
-- rtypeはパターンマッチの右辺の型。フォースが共にあらんことを。
collectTypeOfExprsFromLetPattern :: Gamma -> LetPattern -> TypeExpr -> TypeCheckEff Gamma
collectTypeOfExprsFromLetPattern gm pat@(LetPatternPattern t mpat) rtype = collectTypeOfExprsFromMatchPattern gm mpat
collectTypeOfExprsFromLetPattern gm pat@(FuncLetPattern t f args) rtype = do
  let f' = TypeOfExpr [] f t
  args' <- mapM (\(x, xt) -> pure $ TypeOfExpr [] x xt) args
  pure $ f' : args' <> gm
--  let functype = buildFuncType rtype (fmap snd args)
--  let ftc = TypeEq functype t
--  scms <- genTypeOfExprInLet gm pat
--  pure $ ftc : scms

-- makeClosureOnLetPattern :: Gamma -> LetPattern -> TypeExpr -> TypeCheckEff Gamma
-- makeClosureOnLetPattern gm (FuncLetPattern pt s xs) rt = do 
--   sig <- makeClosure gm s pt
--   xs' <- forM xs $ \(s@(Sym st),t) -> do
--     pure $ TypeOfExpr [] s t
--   pure $ xs' <> [sig, TypeEq pt (buildFuncType rt (snd <$> xs))]
-- makeClosureOnLetPattern gm (LetPatternPattern pt inner) rt = makeClosureOnMatchPattern gm inner rt
-- 
-- makeClosureOnMatchPattern :: Gamma -> Pattern -> TypeExpr -> TypeCheckEff Gamma
-- makeClosureOnMatchPattern gm (ConstantPattern t v) rt = pure [TypeEq t rt]
-- makeClosureOnMatchPattern gm (VarPattern t x) rt = do
--   sig <- makeClosure gm x rt
--   pure [TypeEq t rt, sig]

buildFuncTypeOnLetPattern :: LetPattern -> TypeExpr -> Gamma
buildFuncTypeOnLetPattern (FuncLetPattern ft _ xs) rt = pure $ TypeEq (buildFuncType rt (snd <$> xs)) ft
buildFuncTypeOnLetPattern pat rt = [TypeEq (pat ^. typeExpr_) rt]

buildFuncType :: TypeExpr -> [TypeExpr] -> TypeExpr
buildFuncType ret [] = ret
buildFuncType ret (x:xs) = x ::-> buildFuncType ret xs

collectTypeOfExprsFromMatchPattern :: Gamma -> Pattern -> TypeCheckEff Gamma
collectTypeOfExprsFromMatchPattern gm ConstantPattern {}  = pure [] 
collectTypeOfExprsFromMatchPattern gm ConstrPattern {}  = undefined 
collectTypeOfExprsFromMatchPattern gm ListPattern {}  = pure []
collectTypeOfExprsFromMatchPattern gm OrPattern {} = pure [] 
collectTypeOfExprsFromMatchPattern gm (VarPattern theType sym) = do
  pure $ [TypeOfExpr [] sym theType] <> gm
collectTypeOfExprsFromMatchPattern gm (ParenPattern theType pat) = do
  inner <- collectTypeOfExprsFromMatchPattern gm pat 
  pure $ (TypeEq (pat ^. typeExpr_) theType) : inner <> gm

typeCheckImpl :: Gamma -> TExpr -> TypeCheckEff (Gamma, TExpr)
typeCheckImpl gm (TVar s t) = do
  (TypeOfExpr tvs s' t') <- getClosure gm s t
  t'' <- foldrM (\tv t -> do
    tv' <- genTypeVar
    pure $ replaceTypeVar tv (fromTV tv') t') t' tvs
  pure ([], TVar s t'')
typeCheckImpl gm (TConstant v t) = do
  let tvars = getTypeVar t
  t' <- foldrM (\tv t -> do
                   tv' <- genTypeVar
                   pure $ replaceTypeVar tv (fromTV tv') t) t tvars 
  pure ([], TConstant v t')
typeCheckImpl gm (TLet pat body t) = do
  pts <- collectTypeOfExprsFromLetPattern gm pat (body ^. typeExpr_)
  (s1, t1) <- typeCheckImpl (pts <> gm) body 
  pt <- collectTypeOfExprsFromLetPattern s1 pat (body ^. typeExpr_)
  s2 <- unify $ buildFuncTypeOnLetPattern pat (t1 ^. typeExpr_) <> pt <> s1
  traceM $ show s2
  pure (s2, TLet (applyGamma s2 pat) (applyGamma s2 t1) (applyGamma s2 t1 ^. typeExpr_))
typeCheckImpl gm (TLetIn pat body impl t) = do
  pts <- collectTypeOfExprsFromLetPattern gm pat (body ^. typeExpr_)
  (s1, t1) <- typeCheckImpl (pts <> gm) body 
  (s2, TLet pat body t') <- typeCheckImpl gm (TLet pat body t)
  sig <- collectTypeOfExprsFromLetPattern (applyGamma s2 gm) pat (applyGamma s2 body ^. typeExpr_)
  (s3,t3) <- typeCheckImpl (sig <> gm) impl
  s4 <- unify $ s2 <> s3
  pure (s4, TLetIn (applyGamma s2 pat) (applyGamma s2 t1) (applyGamma s4 t3) (applyGamma s4 t3 ^. typeExpr_))
typeCheckImpl gm (TLetRecIn x body impl t) = do
  (gm', TLetIn x' b' i' t') <- typeCheckImpl gm $ TLetIn x body impl t
  pure (gm', TLetRecIn x' b' i' t') 
typeCheckImpl gm (TIfThenElse c t e ty) = do
  gm' <- unify $ TypeEq (c ^. typeExpr_) ocamlBool : gm
  (cgm, ct) <- typeCheckImpl gm' c
  (tgm, tt) <- typeCheckImpl (TypeEq (t ^. typeExpr_) (e ^. typeExpr_) : cgm) t
  (egm, et) <- typeCheckImpl (TypeEq (t ^. typeExpr_) (e ^. typeExpr_) : cgm) e
  gm'' <- unify (cgm <> tgm <> egm)
  pure (gm, TIfThenElse (applyGamma gm'' ct) (applyGamma gm'' tt) (applyGamma gm'' et) (et ^. typeExpr_))
typeCheckImpl gm e@(TInfixOpExpr l op r t) 
  | op `elem` [Plus, Minus, Mul, Div, Mod] = do
    (lgm, l') <- typeCheckImpl gm l
    (rgm, r') <- typeCheckImpl gm r
    newgm <- unify $ [TypeEq (l' ^. typeExpr_) ocamlInt, TypeEq (r' ^. typeExpr_) ocamlInt, TypeEq t ocamlInt] <> lgm <> rgm <> gm
    pure (newgm, TInfixOpExpr (applyGamma newgm l') op (applyGamma newgm r') ocamlInt)
  | op `elem` [PlusDot, MinusDot, MulDot, DivDot] = do
    (lgm, l') <- typeCheckImpl gm l
    (rgm, r') <- typeCheckImpl gm r
    newgm <- unify $ [TypeEq (l' ^. typeExpr_) ocamlFloat, TypeEq (r' ^. typeExpr_) ocamlFloat, TypeEq (e ^. typeExpr_) ocamlFloat] <> lgm <> rgm <> gm
    pure (newgm, TInfixOpExpr (applyGamma newgm l') op (applyGamma newgm r') ocamlFloat)
  | op `elem` [BoolAnd, BoolOr] = do
    (lgm, l') <- typeCheckImpl gm l
    (rgm, r') <- typeCheckImpl gm r
    newgm <- unify $ [TypeEq (l' ^. typeExpr_) ocamlBool, TypeEq (r' ^. typeExpr_) ocamlBool, TypeEq (e ^. typeExpr_) ocamlBool] <> lgm <> rgm <> gm
    pure (newgm, TInfixOpExpr (applyGamma newgm l') op (applyGamma newgm r') ocamlBool)
  | isComp op = do
    (lgm, l') <- typeCheckImpl gm l
    (rgm, r') <- typeCheckImpl gm r
    newgm <- unify $ [TypeEq (e ^. typeExpr_) ocamlBool, TypeEq (l' ^. typeExpr_) (r' ^. typeExpr_)] <> lgm <> rgm <> gm
    pure (newgm, TInfixOpExpr (applyGamma newgm l') op (applyGamma newgm r') ocamlBool)
  | otherwise =
      throwTypeError $ "oprator" <> (pack . show) op <> "'s type constraint is undefined"
    where
      isComp (Compare _) = True
      isComp _ = False

runTypeCheckEff  :: TypeCheckEff a -> Either CompileError a
runTypeCheckEff = leaveEff . runEitherEff . flip evalStateEff initialGensymState 

typeCheck :: Expr -> Either CompileError TExpr
typeCheck e = do
  e' <- renameSymsByScope e
  let te' = initialTypeInfer e'
  (gm , res) <- runTypeCheckEff $ typeCheckImpl [] te'
  pure $ applyGamma gm res  -- 全てにセットしてしまうのでpatternだけにするなど改装が必要 
