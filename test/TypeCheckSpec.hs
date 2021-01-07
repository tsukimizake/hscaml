{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec where

import Control.Monad.State
import Data.Set as S
import Debug.Trace
import HsCaml.FrontEnd.OCamlType
import HsCaml.FrontEnd.Types
import HsCaml.Parser.Parser
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.TypeCheckUtil
import HsCaml.TypeChecker.TypeChecker
import Test.Hspec

testRSBS :: String -> Expr -> Spec
testRSBS src ast = it src $ do
  let res = renameSymsByScope . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

testTypeCheckExpr :: String -> TExpr -> Spec
testTypeCheckExpr src ast = it src $ do
  let res = typeCheck . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

typeCheckSpec :: Spec
typeCheckSpec = do
  describe "renameSymsByScope" $ do
    testRSBS
      "let x = 1 in let x = 2 in let x = 3 in x"
      ( LetIn
          (LetPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_0")))
          (IntC 1)
          ( LetIn
              (LetPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_1")))
              (IntC 2)
              ( LetIn
                  (LetPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_x_gen_2")))
                  (IntC 3)
                  (V "_x_gen_2")
              )
          )
      )
    testRSBS
      "let rec a = 0 in if a=0 then 2 else 3"
      ( LetRecIn
          (LetPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "_a_gen_0")))
          (Constant (IntVal 0))
          (IfThenElse (InfixOpExpr (Var (Sym "_a_gen_0")) (Compare Equal) (Constant (IntVal 0))) (Constant (IntVal 2)) (Constant (IntVal 3)))
      )
    testRSBS
      "let x = 0 in match x with | 42 -> x"
      ( LetIn
          ( LetPattern
              UnspecifiedType
              (VarPattern UnspecifiedType (Sym "_x_gen_0"))
          )
          (Constant (IntVal 0))
          ( Match
              (Var (Sym "_x_gen_0"))
              [(ConstantPattern UnspecifiedType (IntVal 42), Var (Sym "_x_gen_0"))]
          )
      )
    testRSBS
      "let x = 0 in match x with | 42 -> true"
      ( LetIn
          ( LetPattern
              UnspecifiedType
              (VarPattern UnspecifiedType (Sym "_x_gen_0"))
          )
          (Constant (IntVal 0))
          ( Match
              (Var (Sym "_x_gen_0"))
              [(ConstantPattern UnspecifiedType (IntVal 42), (Constant (BoolVal True)))]
          )
      )
  describe "typecheck" $ do
    testTypeCheckExpr
      "let f x y = x*y in f"
      ( TLetIn
          ( FuncLetPattern
              (ocamlInt ::-> ocamlInt ::-> ocamlInt)
              (Sym "_f_gen_0")
              [(Sym "_x_gen_0", ocamlInt), (Sym "_y_gen_0", ocamlInt)]
          )
          (TVar (Sym "_x_gen_0") ocamlInt :*: TVar (Sym "_y_gen_0") ocamlInt)
          (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
          (ocamlInt ::-> ocamlInt ::-> ocamlInt)
      )
    testTypeCheckExpr
      "let a = 0 in let b  = a in if a =b then 42 else 3"
      ( TLetIn
          (LetPattern ocamlInt (VarPattern ocamlInt (Sym "_a_gen_0")))
          (TIntC 0)
          ( TLetIn
              (LetPattern ocamlInt (VarPattern ocamlInt (Sym "_b_gen_0")))
              (TVar (Sym "_a_gen_0") ocamlInt)
              ( TIfThenElse
                  ((TVar (Sym "_a_gen_0") ocamlInt) :==: (TVar (Sym "_b_gen_0") ocamlInt))
                  (TIntC 42)
                  (TIntC 3)
                  ocamlInt
              )
              ocamlInt
          )
          ocamlInt
      )
    testTypeCheckExpr
      "let f x y z = if x then y else z"
      ( TLet
          ( FuncLetPattern
              (ocamlBool ::-> TypeVar "_4" 0 ::-> TypeVar "_4" 0 ::-> TypeVar "_4" 0)
              (Sym "_f_gen_0")
              [(Sym "_x_gen_0", ocamlBool), (Sym "_y_gen_0", TypeVar "_4" 0), (Sym "_z_gen_0", TypeVar "_4" 0)]
          )
          ( TIfThenElse
              (TVar (Sym "_x_gen_0") ocamlBool)
              (TVar (Sym "_y_gen_0") (TypeVar "_4" 0))
              (TVar (Sym "_z_gen_0") (TypeVar "_4" 0))
              (TypeVar "_4" 0)
          )
          ocamlUnit
      )
    testTypeCheckExpr
      "let f x = x in f 0; f true;"
      ( TLetIn
          ( FuncLetPattern
              (TypeVar "_0" 0 ::-> TypeVar "_0" 0)
              (Sym "_f_gen_0")
              [(Sym "_x_gen_0", TypeVar "_0" 0)]
          )
          (TVar (Sym "_x_gen_0") (TypeVar "_0" 0))
          ( TMultiExpr
              [ TFunApply (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt)) [TIntC 0] ocamlInt,
                TFunApply (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt)) [TBoolC True] ocamlBool
              ]
              ocamlBool
          )
          ocamlBool
      )

-- testTypeCheckExpr "let rec length xs =  match xs with  | [] -> 0  | y::ys -> 1+length ys;;"
--   ()
