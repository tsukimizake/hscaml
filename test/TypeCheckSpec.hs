{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheckSpec where
import Test.Hspec
import HsCaml.FrontEnd.Types
import HsCaml.FrontEnd.OCamlType
import HsCaml.Parser.Parser
import HsCaml.TypeChecker.TypeChecker
import HsCaml.TypeChecker.TypeCheckUtil
import HsCaml.TypeChecker.RenameSymsByScope
import Control.Monad.State
import Debug.Trace
import Data.Set as S
import HsCaml.TypeChecker.CollectTypeConstraints
import HsCaml.TypeChecker.InitialTypeInfer
import HsCaml.TypeChecker.TypeChecker

testRSBS :: String -> Expr -> Spec
testRSBS src ast = it src $ do
  let res = renameSymsByScope . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

testInitialTypeInfer :: String -> TExpr -> Spec
testInitialTypeInfer src ast = it src $ do
  let res = fmap initialTypeInfer . renameSymsByScope . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

testTypeCheckExpr :: String -> TExpr -> Spec
testTypeCheckExpr src ast = it src $ do
  let res = typeCheck . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

testCollectConstraints :: String -> [TypeConstraint] -> Spec
testCollectConstraints s cs = it s $ do
    let constraints = do
          expr <- renameSymsByScope . parseExpr $ s
          let texpr = initialTypeInfer expr
          pure $ collectTypeConstraints texpr
    case constraints of
      Left err -> error $ show err
      Right constraints -> constraints `shouldBe` S.fromList cs

typeCheckSpec :: Spec
typeCheckSpec = do
  describe "renameSymsByScope" $ do
    testRSBS "let x = 1 in let x = 2 in let x = 3 in x" (LetIn
       (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_0")))
       (IntC 1)
       (LetIn (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_1")))
         (IntC 2)
         (LetIn (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_2")))
           (IntC 3)
           (V "_x_gen_2"))))
    testRSBS "let rec a = 0 in if a=0 then 2 else 3"
      (LetRecIn
       (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_a_gen_0")))
       (Constant (IntVal 0))
       (IfThenElse (InfixOpExpr (Var (Sym "_a_gen_0")) (Compare Equal) (Constant (IntVal 0))) (Constant (IntVal 2)) (Constant (IntVal 3))))
    testRSBS "let x = 0 in match x with | 42 -> x"
      (LetIn
      (LetPatternPattern
       UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_0")))
      (Constant (IntVal 0))
      (Match
       (Var (Sym "_x_gen_0"))
       [(ConstantPattern UnspecifiedType (IntVal 42), Var (Sym "_x_gen_0"))]))
    testRSBS
      "let x = 0 in match x with | 42 -> true"
      (LetIn
      (LetPatternPattern
       UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_0")))
      (Constant (IntVal 0))
      (Match
       (Var (Sym "_x_gen_0"))
       [(ConstantPattern UnspecifiedType (IntVal 42), (Constant (BoolVal True)))]))
  describe "initialTypeInfer" $ do
    testInitialTypeInfer "let f x y = x*y in f"
      (TLetIn
        (FuncLetPattern (TypeVar "_0") (Sym "_f_gen_0") [(Sym "_x_gen_0", TypeVar "_1"),(Sym "_y_gen_0", TypeVar "_2")])
        (TInfixOpExpr (TVar (Sym "_x_gen_0") (TypeVar "_1")) Mul (TVar (Sym "_y_gen_0") (TypeVar "_2")) (TypeVar "_3"))
        (TVar (Sym "_f_gen_0") (TypeVar "_0")) (TypeVar "_4"))
  describe "collectTypeInfo" $ do
    testCollectConstraints "let f x y = x*y in f"
      [
        TypeEq (TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_3") (TypeVar"_0")
      , TypeEq (TypeVar "_0") (TypeVar "_4")
      , TypeEq (TypeVar "_1") ocamlInt
      , TypeEq (TypeVar "_2") ocamlInt
      , TypeEq (TypeVar "_3") ocamlInt
      ]

    testCollectConstraints "let rec f g x = f (g x) in g"
      [
        TypeEq(TypeVar "_4" ::-> TypeVar "_5") (TypeVar "_0")
      , TypeEq(TypeVar "_1" ::-> TypeVar "_2" ::-> TypeVar "_5") (TypeVar "_0")
      , TypeEq (TypeVar "_3") (TypeVar "_4")
      , TypeEq (TypeVar "_2" ::-> TypeVar "_3") (TypeVar "_1")
      , TypeEq (TypeVar "_1") (TypeVar "_6")
      ]
  testCollectConstraints "let rec f x = if x = 0 then 1 else x * f (x-1)"
    [
     TypeEq (TypeAtom "int") (TypeVar "_6"),
     TypeEq (TypeAtom "int") (TypeVar "_7"),
     TypeEq (TypeVar "_1" ::-> TypeVar "_7") (TypeVar "_0"),
     TypeEq (TypeVar "_4" ::-> TypeVar "_5") (TypeVar "_0"),
     TypeEq (TypeVar "_1") (TypeAtom "int"),
     TypeEq (TypeVar "_2") (TypeAtom "bool"),
     TypeEq (TypeVar "_3") (TypeAtom "int"),
     TypeEq (TypeVar "_3") (TypeVar "_4"),
     TypeEq (TypeVar "_5") (TypeAtom "int"),
     TypeEq (TypeVar "_6") (TypeAtom "int")]
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
    testTypeCheckExpr "let f x y z = if x then y else z"
       (TLet
         (FuncLetPattern (ocamlBool ::-> TypeVar "_3" ::-> TypeVar "_3" ::-> TypeVar "_4")
          (Sym "_f_gen_0") [(Sym "_x_gen_0", ocamlBool), (Sym "_y_gen_0", TypeVar "_3"), (Sym "_z_gen_0", TypeVar "_3")])
        (TIfThenElse
        (TVar (Sym "_x_gen_0") ocamlBool)
        (TVar (Sym "_y_gen_0") (TypeVar "__gen_2"))
        (TVar (Sym "_z_gen_0") (TypeVar "__gen_2"))
        (TypeVar "__gen_2")
        )
        (TypeVar "__gen_2")
        ) 
    testTypeCheckExpr "let f x = x in f 0; f true;"
      (TLetIn
        (FuncLetPattern (TypeVar "_0" ::-> TypeVar "_0")
          (Sym "_f_gen_0") [(Sym "_x_gen_0", TypeVar "_0")])
        (TVar (Sym "_x_gen_0") (TypeVar "_0"))
        (TMultiExpr [
            TFunApply (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt)) [TIntC 0] ocamlInt,
            TFunApply (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt)) [TBoolC True] ocamlBool
          ] ocamlBool
        ) ocamlBool)

    -- testTypeCheckExpr "let rec length xs =  match xs with  | [] -> 0  | y::ys -> 1+length ys;;"
    --   ()
