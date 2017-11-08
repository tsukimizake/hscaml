{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing, -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, FlexibleContexts #-}
module TypeChecker (typeCheck, renameSymsByScope) where
import Control.Lens
import Types
import OCamlType
import Data.Set as S
import Data.Map as M
import qualified Data.Text as T hiding (head)
import Control.Monad.State.Strict
import Control.Monad.Trans.Either as E
import Control.Monad
import Control.Monad.Skeleton
import RenameSymsByScope
import Data.Monoid

data TypeRestrict =
    OpType {
        __exp :: InfixOp,
        __theType :: TypeExpr
    }|
    SymType {
        __sym :: Sym,
        __theType :: TypeExpr
    }|
    TypeEq{
        __syms :: [Sym]
} deriving (Show, Ord, Eq)

makeLenses ''TypeRestrict



instance AsTExpr Expr where
    _TExpr  = prism toExpr toTExpr
      where
        toExpr :: TExpr -> Expr
        toExpr (TConstant x _)= Constant x
        toExpr (TVar x _) = Var x
        toExpr (TParen x _) = Paren (toExpr x)
        toExpr (TInfixOpExpr x y z _) = InfixOpExpr (toExpr x) y (toExpr z)
        toExpr (TBegEnd x _) = BegEnd (toExpr x)
        toExpr (TMultiExpr x _) = MultiExpr (fmap toExpr x)
        toExpr (TConstr x _) = Constr (toExpr x)
        toExpr (TIfThenElse x y z _) = IfThenElse (toExpr x) (toExpr y) (toExpr z)
        toExpr (TMatch x y _) = Match (toExpr x) (fmap toExpr <$> y)
        toExpr (TWhile x y _) = While (toExpr x) (toExpr y)
        toExpr (TFunApply x y _) = FunApply x (fmap toExpr y)
        toExpr (TLet x y _) = Let x (toExpr y)
        toExpr (TLetRec x y _) = LetRec x (toExpr y)
        toExpr (TLetIn x y z _) = LetIn x (toExpr y) (toExpr z)
        toExpr (TTypeDecl x y _) = TypeDecl x y
        toTExpr :: Expr -> Either Expr TExpr
        toTExpr e =
            let te = initialTypeInfer e
                tet = te ^. _typeExpr
            in case tet of
                   UnspecifiedType -> Left e
                   _ -> Right te

typeCheck :: Expr -> Either CompileError TExpr
typeCheck = runTypeInfer . initialTypeInfer . renameSymsByScope
  where
    runTypeInfer :: TExpr -> Either CompileError TExpr
    runTypeInfer x = do
        let res = evalState (typeInfer x) initialRestriction
        typedExprisValid res

-- とりあえずプリミティブや即値やアノテーション書かれたやつにだけ型を付ける
initialTypeInfer :: Expr -> TExpr
initialTypeInfer (Constant x@(IntVal _)) = TConstant x ocamlInt
initialTypeInfer (Constant x@(BoolVal _)) = TConstant x ocamlBool
initialTypeInfer (Var x) = TVar x UnspecifiedType
initialTypeInfer (Paren e) = TParen (initialTypeInfer e) UnspecifiedType
initialTypeInfer (InfixOpExpr e f g) = TInfixOpExpr
                                       (initialTypeInfer e) f (initialTypeInfer g) UnspecifiedType
initialTypeInfer (BegEnd e) = TBegEnd (initialTypeInfer e) UnspecifiedType
initialTypeInfer (MultiExpr e) = TMultiExpr (fmap initialTypeInfer e) UnspecifiedType
initialTypeInfer (Constr e) = TConstr (initialTypeInfer e) UnspecifiedType
initialTypeInfer (IfThenElse e f g) = TIfThenElse (initialTypeInfer e) (initialTypeInfer f) (initialTypeInfer g) UnspecifiedType
initialTypeInfer (Match e f) = TMatch (initialTypeInfer e) (fmap initialTypeInfer <$> f) UnspecifiedType
initialTypeInfer (While e f) = TWhile (initialTypeInfer e) (initialTypeInfer f) UnspecifiedType
initialTypeInfer (FunApply e f) = TFunApply e (fmap initialTypeInfer f) UnspecifiedType
initialTypeInfer (Let e f) = TLet e (initialTypeInfer f) UnspecifiedType
initialTypeInfer (LetRec e f) = TLetRec e (initialTypeInfer f) UnspecifiedType
initialTypeInfer (LetIn e f g) = TLetIn e (initialTypeInfer f) (initialTypeInfer g) UnspecifiedType
initialTypeInfer (TypeDecl e f) = TTypeDecl e f  UnspecifiedType

-- let x = 0
-- in let x = 1
-- in x
-- =>
-- let x0 = 0
-- in let x1 = 1
-- in x1
-- x1が元からあった時のためのgensymは後で



-- 最初から型がわかっている式の一覧
initialRestriction :: Set TypeRestrict
initialRestriction = S.fromList
    [
      OpType Mul (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType Plus (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType Minus (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType Div (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType MulDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      OpType PlusDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      OpType MinusDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      OpType DivDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      -- ToDo,Compareの型推論が効かない. 多相型を入れる必要があるか
      OpType (Compare LessThan) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType (Compare LessThanEq) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType (Compare GreaterThan) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType (Compare GreaterThanEq) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),

      OpType BoolAnd (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType BoolOr (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType Mod (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool)
    ]

typeInfer :: TExpr -> State (Set TypeRestrict) TExpr
typeInfer = undefined

-- まずシンボルのリネームを先にやったほうがよいのでは？ やった
-- 型が付きうるのは、シンボル、演算子、式. 式はTExprに型が付くから、シンボルと演算子だけメモすればよい
-- letの右辺からの推論
-- let f x y = x*y
-- => f : int->int->int
-- => x, y : int

-- if 左辺 is Unspecified and 右辺 is Unspecified
-- 右辺を掘る
-- if 左辺 xor 右辺 is specified
-- specifyしてUnspecifiedだったほうを型情報Mapに入れる
-- if どっちもspecified
-- 整合チェック
-- patternからの推論
-- let (x:int) = a
-- => x : int

-- 多相型入ったらどうなるんだろーねー？

-- 全て型が付いているかどうかのチェック
typedExprisValid :: TExpr -> Either CompileError TExpr
typedExprisValid = undefined



