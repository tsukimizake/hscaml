{-# OPTIONS -Wall -Wno-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module HsCaml.TypeChecker.CollectTypeConstraints (collectTypeConstraints, collectTypeConstraintsStmt, TypeConstraint(TypeEq)) where
import Control.Lens as L
import HsCaml.Common.Types
import HsCaml.Common.OCamlType
import Control.Monad.State
import Data.Monoid
import HsCaml.TypeChecker.TypeCheckUtil
import Data.Set as S



collectTypeConstraintsStmt :: TStatement -> CollectTypeConstraintsM (S.Set TypeConstraint)
collectTypeConstraintsStmt (TStatement exprs) = do
  forM_ exprs $ \expr -> do
    collectTypeConstraintsImpl expr
  get

type CollectTypeConstraintsM a = State (S.Set TypeConstraint) a

putTypeConstraint :: TypeConstraint -> CollectTypeConstraintsM ()
putTypeConstraint tc = do
  modify' (S.insert tc)

collectFromLetPattern :: LetPattern -> TypeExpr -> CollectTypeConstraintsM (Maybe TypeConstraint) -- パターンがVarPattern/FuncLetPatternのときはそのシンボルの型を返す
                   -- rtypeはパターンマッチの右辺の型。フォースが共にあらんことを。
collectFromLetPattern (LetPatternPattern t mpat) rtype = collectFromMatchPattern mpat rtype
collectFromLetPattern (FuncLetPattern t f args) rtype = do
  let functype = buildFuncType rtype (fmap snd args)
  putTypeConstraint $ TypeEq functype t
  pure $ Nothing
    where
      buildFuncType :: TypeExpr -> [TypeExpr] -> TypeExpr
      buildFuncType ret [] = ret
      buildFuncType ret (x:xs) = x ::-> buildFuncType ret xs

collectFromMatchPattern :: Pattern -> TypeExpr -> CollectTypeConstraintsM (Maybe TypeConstraint)
collectFromMatchPattern (ConstantPattern _ _) _ = pure Nothing
collectFromMatchPattern (ListPattern _ _) _ = pure Nothing
collectFromMatchPattern (OrPattern _ _ _) _ = pure Nothing
collectFromMatchPattern (VarPattern theType sym) rtype = do
  putTypeConstraint $ TypeEq theType rtype
  pure Nothing
collectFromMatchPattern (ParenPattern theType pat) rtype = do
  putTypeConstraint $ TypeEq (pat ^. _patType) theType
  collectFromMatchPattern pat rtype

collectTypeConstraintsImpl :: TExpr -> CollectTypeConstraintsM TExpr
collectTypeConstraintsImpl exp@(TIfThenElse cond fst snd t) = do
  putTypeConstraint $ TypeEq (fst ^. _typeExpr) (snd ^. _typeExpr)
  putTypeConstraint $ TypeEq (cond ^. _typeExpr) ocamlBool
  putTypeConstraint $ TypeEq (fst ^. _typeExpr) t
  pure exp
collectTypeConstraintsImpl exp@(TLetRec pat impl t) = do
  _ <- collectFromLetPattern pat (impl ^. _typeExpr)
  pure exp
collectTypeConstraintsImpl exp@(TLetIn pat impl body t) = do
  _ <- collectFromLetPattern pat (impl ^. _typeExpr)
  putTypeConstraint $ TypeEq (body ^. _typeExpr) t
  pure exp
collectTypeConstraintsImpl exp@(TLetRecIn pat impl body t) = do
  _ <- collectFromLetPattern pat (impl ^. _typeExpr)
  putTypeConstraint $ TypeEq (body ^. _typeExpr) t
  pure exp
collectTypeConstraintsImpl exp@(TFunApply func args t) = do
  let constraintType = makeConstraintType func args t
  putTypeConstraint $ TypeEq constraintType (func ^. _typeExpr)
  pure exp
    where
      makeConstraintType :: TExpr -> [TExpr] -> TypeExpr -> TypeExpr
      makeConstraintType _ [] t = t
      makeConstraintType s (x:xs) t = (x ^. _typeExpr) ::-> makeConstraintType s xs t
collectTypeConstraintsImpl exp@(TParen inner outtype) = do
  putTypeConstraint $ TypeEq (inner ^. _typeExpr) outtype
  _<-collectTypeConstraintsImpl inner
  pure exp
collectTypeConstraintsImpl exp@(TInfixOpExpr l op r t)
  | elem op [Plus, Minus, Mul, Div, Mod] = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) ocamlInt
      putTypeConstraint $ TypeEq (r ^. _typeExpr) ocamlInt
      putTypeConstraint $ TypeEq t ocamlInt
      pure exp
  | elem op [PlusDot, MinusDot, MulDot, DivDot] = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) ocamlFloat
      putTypeConstraint $ TypeEq (r ^. _typeExpr) ocamlFloat
      putTypeConstraint $ TypeEq t ocamlFloat
      pure exp
  | elem op [BoolAnd, BoolOr] = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) ocamlBool
      putTypeConstraint $ TypeEq (r ^. _typeExpr) ocamlBool
      putTypeConstraint $ TypeEq t ocamlBool
      pure exp
  | isComp op = do
      putTypeConstraint $ TypeEq (l ^. _typeExpr) (r ^. _typeExpr)
      putTypeConstraint $ TypeEq t ocamlBool
      pure exp
  | otherwise = do
      error $ "oprator" <> show op <> "'s type constraint is undefined"
    where
      isComp (Compare _) = True
      isComp _ = False
collectTypeConstraintsImpl exp@(TMultiExpr xs t) = do
  let last = xs !! (length xs - 1)
  putTypeConstraint $ TypeEq (last ^. _typeExpr) t
  pure exp
collectTypeConstraintsImpl exp = pure exp

collectTypeConstraints :: TExpr -> Set TypeConstraint
collectTypeConstraints te = let
  impl = traverseTExpr collectTypeConstraintsImpl
  in (impl te) `execState` S.empty
