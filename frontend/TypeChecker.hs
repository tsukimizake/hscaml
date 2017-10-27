{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module TypeChecker (typeCheck) where
import Control.Lens
import Types
import OCamlType


-- 型付け中の構文木
data TypingExpr = TypingExpr{
    _TypingExprtypeExpr :: Maybe TypeExpr,
    _TypingExprbodyExpr ::  Expr
} deriving (Show, Eq)

makeFields ''TypingExpr

typeCheck :: Expr -> TExpr
typeCheck = undefined


-- とりあえずプリミティブや即値やアノテーション書かれたやつにだけ型を付ける
initialTypeInfer :: Expr -> TypingExpr
initialTypeInfer x@(IntC _) = TypingExpr (Just ocamlInt) x
initialTypeInfer x@(BoolC _) = TypingExpr (Just ocamlBool) x
initialTypeInfer x = TypingExpr Nothing x

-- todo parenpattern with typeを引数に取るlet

-- todo match


-- let f x y = x*y
    -- => f : int->int->int
    -- => x, y : int


-- 全て型が付いているかどうかのチェック
typedExprisValid :: TExpr -> Either CompileError TExpr
typedExprisValid = undefined



