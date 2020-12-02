module HsCaml.TypeChecker.UFTypeChecker (uftypeCheck) where

import Data.Map as M
import Debug.Trace
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckUtil
import HsCaml.TypeChecker.UFUtil

type Env = M.Map Sym TypeExpr

occurs :: TV -> TypeExpr -> TypecheckM Bool
occurs = undefined

typeof :: Env -> Expr -> TypecheckM TypeExpr
typeof = undefined

-- gensymしてTVarつくる
newvar :: TypecheckM TypeExpr
newvar = undefined

-- let時にqvarにしたりしなかったりする
gen :: TypeExpr -> TypecheckM TypeExpr
gen = undefined

-- QVar to new TypeVar
inst :: TypeExpr -> TypecheckM TypeExpr
inst = do
  traverseTypeExpr impl
  where
    -- todo いろいろ
    impl (QVar _) = newvar
    impl x = pure x

uftypeCheck :: Expr -> Either CompileError TExpr
uftypeCheck e = undefined
