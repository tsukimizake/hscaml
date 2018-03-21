{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module CollectTypeConstraints where
import Control.Lens as L
import Control.Lens.Operators
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

collectTypeConstraintsImplStmt :: TStatement -> CollectTypeConstraintsM (S.Set TypeConstraint)
collectTypeConstraintsImplStmt (TStatement exprs) = do
  forM_ exprs $ \expr -> do
    collectTypeConstraintsImpl expr
  get

type CollectTypeConstraintsM a = State (S.Set TypeConstraint) a

putTypeConstraint :: TypeConstraint -> CollectTypeConstraintsM ()
putTypeConstraint tc = do
  modify' (S.insert tc)

initialCollectTypeConstaraintsState = S.empty

collectTypeConstraintsImpl :: TExpr -> CollectTypeConstraintsM TExpr
collectTypeConstraintsImpl exp@(TLetIn pat impl body t) = do
  _ <- collectFromPattern pat (impl ^. _typeExpr)
  putTypeConstraint $ TypeEq (body ^. _typeExpr) t
  putTypeConstraint $ TypeOfExpr exp
  collectTypeConstraintsImpl impl
  collectTypeConstraintsImpl body
  pure exp
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

collectTypeConstraintsImpl exp@(TFunApply func args t) = do
  let constraintType = makeConstraintType func args t
  putTypeConstraint $ TypeEq constraintType (func ^. _typeExpr)
  putTypeConstraint $ TypeOfExpr func
  mapM_ (putTypeConstraint . TypeOfExpr) args
  collectTypeConstraintsImpl func
  mapM_ collectTypeConstraintsImpl args
  pure exp
    where
      makeConstraintType :: TExpr -> [TExpr] -> TypeExpr -> TypeExpr
      makeConstraintType _ [] t = t
      makeConstraintType s (x:xs) t = (x ^. _typeExpr) ::-> makeConstraintType s xs t
collectTypeConstraintsImpl exp@(TParen inner outtype) = do
  putTypeConstraint $ TypeEq (inner ^. _typeExpr) outtype
  collectTypeConstraintsImpl inner
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
collectTypeConstraintsImpl exp = pure exp

collectTypeConstraints :: TExpr -> Set TypeConstraint
collectTypeConstraints te = let
  impl = mapMTExpr collectTypeConstraintsImpl
  in (impl te) `execState` initialCollectTypeConstaraintsState
