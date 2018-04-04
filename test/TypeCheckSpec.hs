{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheckSpec where
import Test.Hspec
import Types
import OCamlType
import Parser
import TypeChecker
import TypeCheckUtil
import RenameSymsByScope
import Control.Monad.State
import Debug.Trace
import Data.Set as S
import CollectTypeConstraints
import InitialTypeInfer
import TypeChecker

testTypeCheckExpr :: String -> TExpr -> Spec
testTypeCheckExpr src ast = it src $ do
  let res = typeCheck . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

collectConstraintsSpec :: String -> [TypeConstraint] -> Spec
collectConstraintsSpec s cs = it s $ do
    let expr = renameSymsByScope . parseExpr $ s
    let texpr = initialTypeInfer expr
    let constraints = collectTypeConstraints texpr
    -- traceM $ show texpr
    constraints `shouldBe` S.fromList cs


typeCheckSpec :: Spec
typeCheckSpec = do
  describe "renameSymsByScope" $ it "let x = 1 in let x = 2 in let x = 3 in x" $ do
    (renameSymsByScope . parseExpr $ "let x = 1 in let x = 2 in let x = 3 in x")
      `shouldBe` ((LetIn
                    (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_0")))
                    (IntC 1)
                    (LetIn (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_1")))
                      (IntC 2)
                      (LetIn (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_2")))
                      (IntC 3)
                       (V "_x_gen_2")))) :: Expr)
  describe "initialTypeInfer" $ it "let f x y = x*y in f" $ do
    (initialTypeInfer . renameSymsByScope . parseExpr $ "let f x y = x*y in f")
      `shouldBe` (TLetIn
                   (FuncLetPattern (TypeVar "_0") (Sym {__name = "_f_gen_0"}) [(Sym {__name = "_x_gen_0"}, TypeVar "_1"),(Sym {__name = "_y_gen_0"}, TypeVar "_2")])
                   (TInfixOpExpr (TVar (Sym {__name = "_x_gen_0"}) (TypeVar "_1")) Mul (TVar (Sym {__name = "_y_gen_0"}) (TypeVar "_2")) (TypeVar "_3"))
                   (TVar (Sym {__name = "_f_gen_0"}) (TypeVar "_0")) (TypeVar "_4"))
  describe "collectTypeInfo" $ do
    collectConstraintsSpec "let f x y = x*y in f"
      [
        TypeEq (TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_3") (TypeVar"_0")
      , TypeEq (TypeVar "_0") (TypeVar "_4")
      , TypeEq (TypeVar "_1") ocamlInt
      , TypeEq (TypeVar "_2") ocamlInt
      , TypeEq (TypeVar "_3") ocamlInt
      ]

    collectConstraintsSpec "let f g x = f (g x) in g"
      [
        TypeEq(TypeVar "_4" ::-> TypeVar "_5") (TypeVar "_0")
      , TypeEq(TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_5") (TypeVar "_0")
      , TypeEq (TypeVar "_3") (TypeVar "_4")
      , TypeEq (TypeVar "_2" ::-> TypeVar "_3") (TypeVar "_1")
      , TypeEq (TypeVar "_1") (TypeVar "_6")
      ]
  collectConstraintsSpec "let rec f x = if x = 0 then 1 else x * f (x-1)"
    [TypeEq {__lhs = TypeAtom "int", __rhs = TypeAtom "int"},
     TypeEq {__lhs = TypeAtom "int", __rhs = TypeVar "_6"},
     TypeEq {__lhs = TypeAtom "int", __rhs = TypeVar "_7"},
     TypeEq {__lhs = TypeVar "_1" ::-> TypeVar "_7", __rhs = TypeVar "_0"},
     TypeEq {__lhs = TypeVar "_4" ::-> TypeVar "_5", __rhs = TypeVar "_0"},
     TypeEq {__lhs = TypeVar "_1", __rhs = TypeAtom "int"},
     TypeEq {__lhs = TypeVar "_2", __rhs = TypeAtom "bool"},
     TypeEq {__lhs = TypeVar "_3", __rhs = TypeAtom "int"},
     TypeEq {__lhs = TypeVar "_3", __rhs = TypeVar "_4"},
     TypeEq {__lhs = TypeVar "_5", __rhs = TypeAtom "int"},
     TypeEq {__lhs = TypeVar "_6", __rhs = TypeAtom "int"}]
  describe "typecheck" $ do
    testTypeCheckExpr "let f x y = x*y in f"
      (TLetIn
        (FuncLetPattern
          (ocamlInt ::-> ocamlInt ::-> ocamlInt)
          (Sym "_f_gen_0") [(Sym "_x_gen_0", ocamlInt), (Sym "_y_gen_0", ocamlInt)])
        (TVar (Sym "_x_gen_0") ocamlInt :*: TVar (Sym "_y_gen_0") ocamlInt)
        (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
        (ocamlInt ::-> ocamlInt ::-> ocamlInt))
    testTypeCheckExpr "let a = 0 in let b  = a in if a =b then 42 else 3"
      (TLetIn
       (LetPatternPattern ocamlInt (VarPattern ocamlInt (Sym "_a_gen_0")))
       (TIntC 0)
       (TLetIn
        (LetPatternPattern ocamlInt (VarPattern ocamlInt (Sym "_b_gen_0")))
        (TVar (Sym "_a_gen_0") ocamlInt)
        (TIfThenElse
         ((TVar (Sym "_a_gen_0") ocamlInt) :==: (TVar (Sym "_b_gen_0") ocamlInt))
         (TIntC 42)
         (TIntC 3)
         ocamlInt
        )
        ocamlInt
       )
       ocamlInt
      )
