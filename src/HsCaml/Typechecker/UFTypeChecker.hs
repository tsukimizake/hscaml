{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module HsCaml.TypeChecker.UFTypeChecker (uftypeCheck) where

import qualified Data.Map as M
import HsCaml.FrontEnd.OCamlType
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckEff
import HsCaml.TypeChecker.TypeCheckUtil
import TextShow as T

occurs :: TV -> TypeExpr -> TypecheckEff Bool
occurs v = undefined

type Env = M.Map Sym TypeExpr

typeVarNotFound :: Sym -> CompileError
typeVarNotFound s =
  TypeError $ "sym " <> T.showt s <> "not found"

(!) :: Env -> Sym -> TypecheckEff TypeExpr
env ! s = do
  case env M.!? s of
    Nothing -> do
      throw $ typeVarNotFound s
    Just r -> inst r

typeof :: Env -> TExpr -> TypecheckEff TypeExpr
typeof _ (TIntC _) = pure ocamlInt
typeof _ (TBoolC _) = pure ocamlBool
typeof env (TVar s _) = do
  inst =<< env ! s
typeof env (TLet pat rhs _) = do
  enterLevel
  tpat <- newvar
  -- TODO funcpattern, listpattern, constrpattern
  trhs <- typeof env rhs
  leaveLevel
  pure ocamlUnit
typeof env (TLetIn pat rhs body _) = do
  enterLevel
  tpat <- newvar
  trhs <- typeof env rhs
  let bodyenv =
        case pat of
          -- TODO funcpattern, listpattern, constrpattern
          LetPattern _ (VarPattern _ s) -> M.insert s tpat env
          _ -> env

  tbody <- typeof bodyenv body
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
uftypeCheck e = runTypecheckEff $ do
  traverseTExpr
    ( \x -> do
        t <- typeof M.empty x
        pure x{typeExpr = t}
    )
    =<< toTExpr e
