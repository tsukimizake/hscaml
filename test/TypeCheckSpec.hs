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
    let texpr = (initialTypeInfer expr) `evalState` initialMangleTypeVarStat
    let constraints = (collectTypeConstraints texpr) `execState` initialCollectTypeConstaraintsState

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
    (initialTypeInfer . renameSymsByScope . parseExpr $ "let f x y = x*y in f") `evalState` initialMangleTypeVarStat
      `shouldBe` (TLetIn
                   (FuncPattern {__patType = TypeVar "_0", __sym = Sym {__name = "_f_gen_0"}, __args = [(Sym {__name = "_x_gen_0"}, TypeVar "_1"),(Sym {__name = "_y_gen_0"}, TypeVar "_2")]})
                   (TInfixOpExpr (TVar (Sym {__name = "_x_gen_0"}) (TypeVar "_1")) Mul (TVar (Sym {__name = "_y_gen_0"}) (TypeVar "_2")) (TypeVar "_3"))
                   (TVar (Sym {__name = "_f_gen_0"}) (TypeVar "_0")) (TypeVar "_4"))
  describe "collectTypeInfo" $ do
    collectConstraintsSpec "let f x y = x*y in f"
      [ TypeOfSym (Sym "_f_gen_0") (TypeVar "_0")
      , TypeOfSym (Sym "_x_gen_0") (TypeVar "_1")
      , TypeOfSym (Sym "_y_gen_0") (TypeVar "_2")
      , TypeEq (TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_4") (TypeVar"_0")
      , TypeEq (TypeVar "_1") ocamlInt
      , TypeEq (TypeVar "_2") ocamlInt
      , TypeEq (TypeVar "_3") ocamlInt
      , TypeEq (TypeVar "_3") (TypeVar "_4")
      ]
    collectConstraintsSpec "let f g x = f (g x) in g"
      [
        TypeOfSym (Sym "_f_gen_0") (TypeVar "_0")
      , TypeOfSym (Sym "_g_gen_0") (TypeVar "_1")
      , TypeOfSym (Sym "_h_gen_0") (TypeVar "_2")
      ]
  describe "typecheck" $ do
    testTypeCheckExpr "let f x y = x*y in f"
      (TLetIn
        (FuncPattern
          (ocamlInt ::-> ocamlInt ::-> ocamlInt)
          (Sym "_f_gen_0") [(Sym "_x_gen_0", ocamlInt), (Sym "_y_gen_0", ocamlInt)])
        (TVar (Sym "_x_gen_0") ocamlInt :*: TVar (Sym "_y_gen_0") ocamlInt)
        (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
        (ocamlInt ::-> ocamlInt ::-> ocamlInt))
