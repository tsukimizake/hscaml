{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

import Data.Text
import GHC.Exts
import HsCaml.FrontEnd.OCamlType
import HsCaml.FrontEnd.Types as Types
import HsCaml.Parser.Parser
import Test.Hspec

testExprParser :: String -> Expr -> Spec
testExprParser lhs rhs = it lhs $ do
  parseTopLevel lhs `shouldBe` (TopLevelExpr rhs)

testTypeDeclParser :: String -> TypeDecl -> Spec
testTypeDeclParser lhs rhs = it lhs $ do
  parseTopLevel lhs `shouldBe` (TopLevelTypeDecl rhs)

parserSpec :: Spec
parserSpec = do
  describe "parseExpr" $ do
    testExprParser "82*3+3-(2-  2)+  (2  )  " ((((IntC 82) :* (IntC 3)) :+ (IntC 3)) :- (Paren ((IntC 2) :- (IntC 2)) UnspecifiedType) :+ (Paren (IntC 2) UnspecifiedType))
    testExprParser "x*y-1" (((V "x") :* (V "y")) :- (IntC 1))
    testExprParser "x * f y -1" ((V "x" :* (FunApply (V "f") [V "y"] UnspecifiedType) :- (IntC 1)))
    testExprParser "x - f y *1" (V "x" :- (((FunApply (V "f") [V "y"] UnspecifiedType) :* (IntC 1))))

    testExprParser "let eu = 4" (Let (LetPattern UnspecifiedType (VarPattern UnspecifiedType "eu")) (Constant (IntVal 4) UnspecifiedType) UnspecifiedType)
    testExprParser "let (eu:'a) = 4" (Let (LetPattern UnspecifiedType (ParenPattern (TypeVar "'a" 0) (VarPattern (TypeVar "'a" 0) "eu"))) (IntC 4) UnspecifiedType)
    testExprParser "let rec f x = x*x*x" (LetRec (FuncLetPattern UnspecifiedType "f" [("x", UnspecifiedType)]) ((V "x" :* V "x" :* V "x")) UnspecifiedType)
    testExprParser "let (eu:int) = 4" (Let (LetPattern UnspecifiedType (ParenPattern ocamlInt (VarPattern ocamlInt "eu"))) (Constant (IntVal 4) UnspecifiedType) UnspecifiedType)
    testExprParser "let rec eu = 4" (LetRec (LetPattern UnspecifiedType (VarPattern UnspecifiedType "eu")) (Constant (IntVal 4) UnspecifiedType) UnspecifiedType)
    testExprParser
      "let rec (eu : int  ) = 4"
      (LetRec (LetPattern UnspecifiedType (ParenPattern ocamlInt (VarPattern ocamlInt "eu"))) (IntC 4) UnspecifiedType)
    testExprParser
      "if x then y else z"
      (IfThenElse (V "x") (V "y") (V "z") UnspecifiedType)
    testExprParser
      "let rec fib x = if x<=1 then 1 else fib (x-1)+fib(x-2)"
      ( LetRec
          (FuncLetPattern UnspecifiedType "fib" [("x", UnspecifiedType)])
          ( IfThenElse
              ((V "x") :<= (IntC 1))
              (IntC 1)
              ( (FunApply (V "fib") [(Paren (V "x" :- IntC 1) UnspecifiedType)] UnspecifiedType)
                  :+ (FunApply (V "fib") [(Paren (V "x" :- IntC 2) UnspecifiedType)] UnspecifiedType)
              )
              UnspecifiedType
          )
          UnspecifiedType
      )
    testExprParser
      "let f a b = a = b"
      ( Let
          (FuncLetPattern UnspecifiedType "f" [("a", UnspecifiedType), ("b", UnspecifiedType)])
          ((V "a") :== (V "b"))
          UnspecifiedType
      )
    testTypeDeclParser "type hoge = Hoge" (TypeDecl "hoge" [DataCnstr "Hoge" []])
    testTypeDeclParser
      "type hoge = Hoge of hoge"
      (TypeDecl "hoge" [DataCnstr "Hoge" [(TypeAtom "hoge")]])
    testTypeDeclParser
      "type hoge = Hoge of hoge | Huga of hoge"
      ( TypeDecl
          "hoge"
          [ DataCnstr "Hoge" [(TypeAtom "hoge")],
            DataCnstr "Huga" [(TypeAtom "hoge")]
          ]
      )
    testTypeDeclParser
      "type hoge = Hoge of hoge * hoge"
      ( TypeDecl
          "hoge"
          [DataCnstr "Hoge" [(TypeAtom "hoge"), (TypeAtom "hoge")]]
      )
    testTypeDeclParser
      "type hoge = Hoge of hoge*hoge"
      (TypeDecl "hoge" [DataCnstr "Hoge" [(TypeAtom "hoge"), (TypeAtom "hoge")]])
    testTypeDeclParser
      "type expr = Plus of expr * expr| Minus of expr * expr | Times of expr * expr | Divide of expr * expr | Value of string"
      ( TypeDecl
          "expr"
          [ DataCnstr "Plus" [(TypeAtom "expr"), (TypeAtom "expr")],
            DataCnstr "Minus" [(TypeAtom "expr"), (TypeAtom "expr")],
            DataCnstr "Times" [(TypeAtom "expr"), (TypeAtom "expr")],
            DataCnstr "Divide" [(TypeAtom "expr"), (TypeAtom "expr")],
            DataCnstr "Value" [TypeAtom "string"]
          ]
      )
    testTypeDeclParser
      "type 'a list = Nil | Cons of 'a * 'a list"
      ( TypeDecl
          "list"
          [ DataCnstr "Nil" [],
            DataCnstr "Cons" [(TypeVar "'a" 0), TypeApplication [(TypeVar "'a" 0)] (TypeAtom "list")]
          ]
      )
    testExprParser
      "let main = print_int 42"
      (Let (LetPattern UnspecifiedType (VarPattern UnspecifiedType "main")) (FunApply (V "print_int") [(IntC 42)] UnspecifiedType) UnspecifiedType)
    testExprParser
      "match x with |1 -> true |2 ->false"
      ( Match
          (V "x")
          [ (ConstantPattern UnspecifiedType (IntVal 1), (Constant (BoolVal True) UnspecifiedType)),
            (ConstantPattern UnspecifiedType (IntVal 2), (Constant (BoolVal False) UnspecifiedType))
          ]
          UnspecifiedType
      )
    testExprParser
      "let f x y = x*y in f"
      ( LetIn
          ( FuncLetPattern
              UnspecifiedType
              "f"
              [("x", UnspecifiedType), ("y", UnspecifiedType)]
          )
          ((V "x") :* (V "y"))
          (V "f")
          UnspecifiedType
      )
    testExprParser
      "let f x y = x*y in 1;2;f;"
      ( LetIn
          ( FuncLetPattern
              UnspecifiedType
              "f"
              [("x", UnspecifiedType), ("y", UnspecifiedType)]
          )
          ((V "x") :* (V "y"))
          ( MultiExpr
              [ IntC 1,
                IntC 2,
                (V "f")
              ]
              UnspecifiedType
          )
          UnspecifiedType
      )
    testExprParser
      "let f g h x = (f g) (h x) in g"
      ( LetIn
          ( FuncLetPattern
              UnspecifiedType
              "f"
              [("g", UnspecifiedType), ("h", UnspecifiedType), ("x", UnspecifiedType)]
          )
          (FunApply (Paren (FunApply (V "f") [(V "g")] UnspecifiedType) UnspecifiedType) [(Paren (FunApply (V "h") [(V "x")] UnspecifiedType) UnspecifiedType)] UnspecifiedType)
          (V "g")
          UnspecifiedType
      )
    testExprParser
      "fun x -> x+1"
      ( LetIn
          ( FuncLetPattern
              UnspecifiedType
              "fun"
              [("x", UnspecifiedType)]
          )
          (V "x" :+ IntC 1)
          (V "x" :+ IntC 1)
          UnspecifiedType
      )
    testExprParser
      "fun x -> x+y"
      ( LetIn
          ( FuncLetPattern
              UnspecifiedType
              "fun"
              [("x", UnspecifiedType)]
          )
          (V "x" :+ V "y")
          (V "x" :+ V "y")
          UnspecifiedType
      )
    testExprParser
      "[a;3;b;]"
      ( Types.List
          [ V "a",
            IntC 3,
            V "b"
          ]
          UnspecifiedType
      )
    testExprParser
      "[|a;3;b;|]"
      ( Array
          [ V "a",
            IntC 3,
            V "b"
          ]
          UnspecifiedType
      )
  describe "parseStatement" $ do
    it "hoge ;; huga ;; " $
      parseStatement "hoge ;; huga ;; "
        `shouldBe` (Statement [TopLevelExpr $ V "hoge", TopLevelExpr $ V "huga"])

-- fun x -> x+1
-- fun x y -> x + y
