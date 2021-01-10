module HsCaml.TypeChecker.UFTypeChecker (uftypeCheck) where

import Data.Map as M
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckEff
import HsCaml.TypeChecker.TypeCheckUtil
import HsCaml.TypeChecker.UFUtil

type Env = M.Map Sym TypeExpr

occurs :: TV -> TypeExpr -> TypecheckEff Bool
occurs = undefined

typeof :: Env -> Expr -> TypecheckEff TypeExpr
typeof = undefined

-- gensymしてTVarつくる
newvar :: Level -> TypecheckEff TypeExpr
newvar l = do
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
    impl (QVar _) = newvar 0
    impl x = pure x

uftypeCheck :: Expr -> Either CompileError TExpr
uftypeCheck e = undefined
