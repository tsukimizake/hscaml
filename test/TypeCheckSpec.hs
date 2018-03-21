{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheckSpec where
import Test.Hspec
import Types
import OCamlType
import Parser
import TypeChecker
import TypeCheckUtil
import Control.Monad.State
import Debug.Trace
import Data.Set as S

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
                    (VarPattern UnspecifiedType (Sym "_x_gen_0"))
                    (IntC 1)
                    (LetIn (VarPattern UnspecifiedType (Sym "_x_gen_1"))
                      (IntC 2)
                      (LetIn (VarPattern UnspecifiedType (Sym "_x_gen_2"))
                      (IntC 3)
                       (V "_x_gen_2")))) :: Expr)
  describe "initialTypeInfer" $ it "let f x y = x*y in f" $ do
    (initialTypeInfer . renameSymsByScope . parseExpr $ "let f x y = x*y in f")
      `shouldBe` (TLetIn
                   (FuncPattern {__patType = TypeVar "_0", __sym = Sym {__name = "_f_gen_0"}, __args = [(Sym {__name = "_x_gen_0"}, TypeVar "_1"),(Sym {__name = "_y_gen_0"}, TypeVar "_2")]})
                   (TInfixOpExpr (TVar (Sym {__name = "_x_gen_0"}) (TypeVar "_1")) Mul (TVar (Sym {__name = "_y_gen_0"}) (TypeVar "_2")) (TypeVar "_3"))
                   (TVar (Sym {__name = "_f_gen_0"}) (TypeVar "_0")) (TypeVar "_4"))
  describe "collectTypeInfo" $ do
    collectConstraintsSpec "let f x y = x*y in f"
      [ TypeOfExpr (TVar (Sym "_f_gen_0") (TypeVar "_0"))
      , TypeOfExpr (TVar (Sym "_x_gen_0") (TypeVar "_1"))
      , TypeOfExpr (TVar (Sym "_y_gen_0") (TypeVar "_2"))
      , TypeEq (TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_3") (TypeVar"_0")
      , TypeEq (TypeVar "_0") (TypeVar "_4")
      , TypeEq (TypeVar "_1") ocamlInt
      , TypeEq (TypeVar "_2") ocamlInt
      , TypeEq (TypeVar "_3") ocamlInt
      ,TypeOfExpr (TLetIn
                   (FuncPattern {__patType = TypeVar "_0", __sym = Sym {__name = "_f_gen_0"}, __args = [(Sym {__name = "_x_gen_0"}, TypeVar "_1"),(Sym {__name = "_y_gen_0"}, TypeVar "_2")]})
                   (TInfixOpExpr (TVar (Sym {__name = "_x_gen_0"}) (TypeVar "_1")) Mul (TVar (Sym {__name = "_y_gen_0"}) (TypeVar "_2")) (TypeVar "_3"))
                   (TVar (Sym {__name = "_f_gen_0"}) (TypeVar "_0")) (TypeVar "_4"))
      ]

    collectConstraintsSpec "let f g x = f (g x) in g"
      [
        TypeOfExpr (TVar (Sym "_f_gen_0") (TypeVar "_0"))
      , TypeOfExpr (TVar (Sym "_g_gen_0") (TypeVar "_1"))
      , TypeOfExpr (TVar (Sym "_x_gen_0") (TypeVar "_2"))
      , TypeOfExpr
        (TLetIn
         (FuncPattern {__patType = TypeVar "_0", __sym = Sym {__name = "_f_gen_0"}, __args = [(Sym {__name = "_g_gen_0"},TypeVar "_1"),(Sym {__name = "_x_gen_0"},TypeVar "_2")]})
         (TFunApply (TVar (Sym "_f_gen_0") (TypeVar "_0"))
          [TParen (TFunApply (TVar (Sym "_g_gen_0") (TypeVar "_1")) [TVar (Sym {__name = "_x_gen_0"}) (TypeVar "_2")] (TypeVar "_3")) (TypeVar "_4")]
          (TypeVar "_5"))
         (TVar (Sym {__name = "_g_gen_0"}) (TypeVar "_1")) (TypeVar "_6"))
      , TypeOfExpr (TParen (TFunApply (TVar (Sym "_g_gen_0") (TypeVar "_1")) [TVar (Sym {__name = "_x_gen_0"}) (TypeVar "_2")] (TypeVar "_3")) (TypeVar "_4"))
      , TypeEq(TypeVar "_4" ::-> TypeVar "_5") (TypeVar "_0")
      , TypeEq(TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_5") (TypeVar "_0")
      , TypeEq (TypeVar "_3") (TypeVar "_4")
      , TypeEq (TypeVar "_2" ::-> TypeVar "_3") (TypeVar "_1")
      , TypeEq (TypeVar "_1") (TypeVar "_6")
      ]
  collectConstraintsSpec "let rec f x = if x = 0 then 1 else x * f (x-1)"
    []
  describe "typecheck" $ do
    testTypeCheckExpr "let f x y = x*y in f"
      (TLetIn
        (FuncPattern
          (ocamlInt ::-> ocamlInt ::-> ocamlInt)
          (Sym "_f_gen_0") [(Sym "_x_gen_0", ocamlInt), (Sym "_y_gen_0", ocamlInt)])
        (TVar (Sym "_x_gen_0") ocamlInt :*: TVar (Sym "_y_gen_0") ocamlInt)
        (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
        (ocamlInt ::-> ocamlInt ::-> ocamlInt))
