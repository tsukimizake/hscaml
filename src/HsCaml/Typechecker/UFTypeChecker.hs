{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module HsCaml.TypeChecker.UFTypeChecker (uftypeCheck) where

import HsCaml.FrontEnd.OCamlType
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckEff
import HsCaml.TypeChecker.TypeCheckUtil

occurs :: TV -> TypeExpr -> TypecheckEff Bool
occurs v = undefined

typeof :: Env -> Expr -> TypecheckEff TypeExpr
typeof _ (IntC _) = pure ocamlInt
typeof _ (BoolC _) = pure ocamlBool
typeof _ (Var s) = inst =<< findEnv s
typeof env (Let pat rhs) = do
  enterLevel
  tpat <- newvar
  trhs <- typeof rhs
  leaveLevel
  pure ocamlUnit
typeof env (LetIn pat rhs body) = do
  enterLevel
  leaveLevel
  pure ocamlUnit

-- gensymしてTVarつくる
newvar :: TypecheckEff TypeExpr
newvar = do
  l <- currentLevel
  flip TypeVar l <$> genSym ""

-- let時にqvarにしたりしなかったりする
gen :: TypeExpr -> TypecheckEff TypeExpr
gen = undefined

-- QVar to new TypeVar
inst :: TypeExpr -> TypecheckEff TypeExpr
inst = do
  traverseTypeExpr impl
  where
    -- todo いろいろ
    impl :: TypeExpr -> TypecheckEff TypeExpr
    impl (QVar _) = newvar
    impl x = pure x

uftypeCheck :: Expr -> Either CompileError TExpr
uftypeCheck e = undefined
