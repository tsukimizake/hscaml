{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec where

import HsCaml.Common.Gensym as GS
import HsCaml.FrontEnd.OCamlType
import HsCaml.FrontEnd.Types
import HsCaml.Parser.Parser
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.TypeChecker
import Test.Hspec

testRSBS :: String -> Expr -> Spec
testRSBS src ast = it src $ do
  let res = renameSymsByScope GS.initialGensymState . parseExpr $ src
  case res of
    Left err -> error $ show err
    Right res' -> res' `shouldBe` ast

testTypeCheckExpr :: String -> Expr -> Spec
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
          (LetPattern UnspecifiedType (VarPattern UnspecifiedType "_x_gen_0"))
          (IntC 1)
          ( LetIn
              (LetPattern UnspecifiedType (VarPattern UnspecifiedType "_x_gen_1"))
              (IntC 2)
              ( LetIn
                  (LetPattern UnspecifiedType (VarPattern UnspecifiedType "_x_gen_2"))
                  (IntC 3)
                  (V "_x_gen_2")
                  UnspecifiedType
              )
              UnspecifiedType
          )
          UnspecifiedType
      )
    testRSBS
      "let rec a = 0 in if a=0 then 2 else 3"
      ( LetRecIn
          (LetPattern UnspecifiedType (VarPattern UnspecifiedType "_a_gen_0"))
          (Constant (IntVal 0) UnspecifiedType)
          (IfThenElse (InfixOpExpr (Var "_a_gen_0" UnspecifiedType) (Compare Equal) (Constant (IntVal 0) UnspecifiedType) UnspecifiedType) (Constant (IntVal 2) UnspecifiedType) (Constant (IntVal 3) UnspecifiedType) UnspecifiedType)
          UnspecifiedType
      )
    testRSBS
      "let x = 0 in match x with | 42 -> x"
      ( LetIn
          ( LetPattern
              UnspecifiedType
              (VarPattern UnspecifiedType "_x_gen_0")
          )
          (Constant (IntVal 0) UnspecifiedType)
          ( Match
              (Var "_x_gen_0" UnspecifiedType)
              [(ConstantPattern UnspecifiedType (IntVal 42), Var "_x_gen_0" UnspecifiedType)]
              UnspecifiedType
          )
          UnspecifiedType
      )
    testRSBS
      "let x = 0 in match x with | 42 -> true"
      ( LetIn
          ( LetPattern
              UnspecifiedType
              (VarPattern UnspecifiedType "_x_gen_0")
          )
          (Constant (IntVal 0) UnspecifiedType)
          ( Match
              (Var "_x_gen_0" UnspecifiedType)
              [(ConstantPattern UnspecifiedType (IntVal 42), (Constant (BoolVal True) UnspecifiedType))]
              UnspecifiedType
          )
          UnspecifiedType
      )
  describe "typecheck" $ do
    testTypeCheckExpr
      "let x = 1 in let y= x in y"
      ( LetIn
          ( LetPattern
              ocamlInt
              (VarPattern ocamlInt "_x_gen_0")
          )
          (TIntC 1)
          (LetIn (LetPattern ocamlInt (VarPattern ocamlInt "_y_gen_0")) (tIntVar "_x_gen_0") (tIntVar "_y_gen_0") ocamlInt)
          ocamlInt
      )
    testTypeCheckExpr
      "let f x y = x*y in f"
      ( LetIn
          ( FuncLetPattern
              (ocamlInt ::-> ocamlInt ::-> ocamlInt)
              "_f_gen_0"
              [(Sym "_x_gen_0", ocamlInt), (Sym "_y_gen_0", ocamlInt)]
          )
          (Var "_x_gen_0" ocamlInt :*: Var "_y_gen_0" ocamlInt)
          (Var "_f_gen_0" (ocamlInt ::-> ocamlInt ::-> ocamlInt))
          (ocamlInt ::-> ocamlInt ::-> ocamlInt)
      )
    testTypeCheckExpr
      "let a = 0 in let b  = a in if a =b then 42 else 3"
      ( LetIn
          (LetPattern ocamlInt (VarPattern ocamlInt "_a_gen_0"))
          (TIntC 0)
          ( LetIn
              (LetPattern ocamlInt (VarPattern ocamlInt "_b_gen_0"))
              (Var "_a_gen_0" ocamlInt)
              ( IfThenElse
                  ((Var "_a_gen_0" ocamlInt) :==: (Var "_b_gen_0" ocamlInt))
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
      ( Let
          ( FuncLetPattern
              (ocamlBool ::-> TypeVar "_4" 0 ::-> TypeVar "_4" 0 ::-> TypeVar "_4" 0)
              "_f_gen_0"
              [(Sym "_x_gen_0", ocamlBool), (Sym "_y_gen_0", TypeVar "_4" 0), (Sym "_z_gen_0", TypeVar "_4" 0)]
          )
          ( IfThenElse
              (Var "_x_gen_0" ocamlBool)
              (Var "_y_gen_0" (TypeVar "_4" 0))
              (Var "_z_gen_0" (TypeVar "_4" 0))
              (TypeVar "_4" 0)
          )
          ocamlUnit
      )
    testTypeCheckExpr
      "let f x = x in f 0; f true;"
      ( LetIn
          ( FuncLetPattern
              (TypeVar "_0" 0 ::-> TypeVar "_0" 0)
              "_f_gen_0"
              [(Sym "_x_gen_0", TypeVar "_0" 0)]
          )
          (Var "_x_gen_0" (TypeVar "_0" 0))
          ( MultiExpr
              [ FunApply (Var "_f_gen_0" (ocamlInt ::-> ocamlInt)) [TIntC 0] ocamlInt,
                FunApply (Var "_f_gen_0" (ocamlInt ::-> ocamlInt)) [TBoolC True] ocamlBool
              ]
              ocamlBool
          )
          ocamlBool
      )

-- testTypeCheckExpr "let rec length xs =  match xs with  | [] -> 0  | y::ys -> 1+length ys;;"
--   ()
