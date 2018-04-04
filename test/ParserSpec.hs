{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec(parserSpec) where
import Test.Hspec
import Data.Text
import Parser
import Types
import OCamlType
import GHC.Exts

instance IsString Sym where
    fromString x = Sym $ pack x

testParser :: String -> Expr -> Spec
testParser lhs rhs= it lhs $ do
        parseExpr lhs `shouldBe` rhs


parserSpec :: Spec
parserSpec = do
    describe "parseExpr" $ do
        testParser "82*3+3-(2-  2)+  (2  )  " ((((IntC 82) :* (IntC 3)) :+ (IntC 3)) :- (Paren ((IntC 2) :- (IntC 2))) :+ (Paren (IntC 2)))
        testParser "x*y-1" (((V "x") :* (V "y")) :- (IntC 1))
        testParser "x * f y -1" ((V "x" :* (FunApply (V "f") [V "y"])) :- (IntC 1))
        testParser "x - f y *1" (V "x" :- ((FunApply (V "f") [V "y"]) :* (IntC 1)))

        testParser "let eu = 4" (Let (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType "eu")) (Constant (IntVal 4)))
        testParser "let (eu:'a) = 4" (Let (LetPatternPattern UnspecifiedType (ParenPattern (TypeVar "'a") (VarPattern (TypeVar "'a") "eu"))) (IntC 4))
        testParser "let rec f x = x*x*x" (LetRec (FuncLetPattern UnspecifiedType "f" [("x", UnspecifiedType)]) ((V "x" :* V "x") :* V "x"))
        testParser "let (eu:int) = 4" (Let (LetPatternPattern UnspecifiedType (ParenPattern ocamlInt (VarPattern ocamlInt "eu"))) (Constant (IntVal 4)))
        testParser "let rec eu = 4" (LetRec (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType "eu")) (Constant (IntVal 4)))
        testParser "let rec (eu : int  ) = 4"
            (LetRec (LetPatternPattern UnspecifiedType (ParenPattern ocamlInt (VarPattern ocamlInt "eu"))) (IntC 4))
        testParser "if x then y else z"
            (IfThenElse (V "x") (V "y") (V "z"))
        testParser "let rec fib x = if x<=1 then 1 else fib (x-1)+fib(x-2)"
            (LetRec (FuncLetPattern UnspecifiedType "fib" [("x", UnspecifiedType)])
                    (IfThenElse ((V "x") :<= (IntC 1))
                     (IntC 1)
                     ((FunApply (V "fib") [(Paren $ V "x" :- IntC 1)])
                      :+
                      (FunApply (V "fib") [(Paren $ V "x" :- IntC 2)]))))
        testParser "let f a b = a = b"
            (Let (FuncLetPattern UnspecifiedType "f" [(Sym "a", UnspecifiedType), (Sym "b", UnspecifiedType)])
             ((V "a") :== (V "b")))
        testParser "type hoge = Hoge" (TypeDecl "hoge" [DataCnstr "Hoge" []])
        testParser "type hoge = Hoge of hoge"
            (TypeDecl "hoge" [DataCnstr "Hoge" [(TypeAtom "hoge")]])
        testParser "type hoge = Hoge of hoge | Huga of hoge"
            (TypeDecl "hoge" [DataCnstr "Hoge" [(TypeAtom "hoge")],
                              DataCnstr "Huga" [(TypeAtom "hoge")]])
        testParser "type hoge = Hoge of hoge * hoge"
            (TypeDecl "hoge"
             [DataCnstr "Hoge" [(TypeAtom "hoge"), (TypeAtom "hoge")]])
        testParser "type hoge = Hoge of hoge*hoge"
            (TypeDecl "hoge" [DataCnstr "Hoge" [(TypeAtom "hoge"), (TypeAtom "hoge")]])
        testParser "type expr = Plus of expr * expr| Minus of expr * expr | Times of expr * expr | Divide of expr * expr | Value of string"
            (TypeDecl "expr"
             [DataCnstr "Plus" [(TypeAtom "expr"), (TypeAtom "expr")],
              DataCnstr "Minus" [(TypeAtom "expr"), (TypeAtom "expr")],
              DataCnstr "Times" [(TypeAtom "expr"), (TypeAtom "expr")],
              DataCnstr "Divide" [(TypeAtom "expr"), (TypeAtom "expr")],
              DataCnstr "Value" [TypeAtom "string"]])
        testParser "type 'a list = Nil | Cons of 'a * 'a list"
            (TypeDecl "list"
                      [DataCnstr "Nil" [],
                       DataCnstr "Cons" [(TypeVar "'a"), TypeApplication [(TypeVar "'a")] (TypeAtom "list")]]
                      )
        testParser "let main = print_int 42"
            (Let (LetPatternPattern UnspecifiedType (VarPattern UnspecifiedType (Sym "main"))) (FunApply (V "print_int") [(IntC 42)]))
        testParser "match x with |1 -> true |2 ->false"
            (Match (V "x") [(ConstantPattern UnspecifiedType (IntVal 1), (Constant(BoolVal True))),
                            (ConstantPattern UnspecifiedType (IntVal 2), (Constant(BoolVal False)))])
        testParser "let f x y = x*y in f"
            (LetIn
              (FuncLetPattern
               UnspecifiedType
               (Sym "f") [(Sym "x", UnspecifiedType),  (Sym "y", UnspecifiedType)])
              ((V "x") :* (V "y"))
              (V "f"))
        testParser "let f x y = x*y in 1;2;f;"
          (LetIn
              (FuncLetPattern
               UnspecifiedType
               (Sym "f") [(Sym "x", UnspecifiedType),  (Sym "y", UnspecifiedType)])
              ((V "x") :* (V "y"))
              (MultiExpr
               [
                 IntC 1,
                 IntC 2,
                 (V "f")]))
        testParser "let f g h x = (f g) (h x) in g"
          (LetIn
          (FuncLetPattern
           UnspecifiedType
          (Sym "f") [(Sym "g", UnspecifiedType), (Sym "h", UnspecifiedType), (Sym "x", UnspecifiedType)])
          (FunApply (Paren (FunApply (V "f") [(V "g")])) [(Paren (FunApply (V "h") [(V "x")]))])
          (V "g"))
        testParser "fun x -> x+1"
          (LetIn
           (FuncLetPattern
            UnspecifiedType
            (Sym "fun") [(Sym "x", UnspecifiedType)])
           (V "x" :+ IntC 1)
           (V "x" :+ IntC 1)
          )
        testParser "fun x -> x+y"
          (LetIn
           (FuncLetPattern
            UnspecifiedType
            (Sym "fun") [(Sym "x", UnspecifiedType)])
           (V "x" :+ V "y")
           (V "x" :+ V "y")
          )
        testParser "[a;3;b;]"
          (Types.List
          [
            V "a",
            IntC 3,
            V "b"
          ])
        testParser "[|a;3;b;|]"
          (Array
          [
            V "a",
            IntC 3,
            V "b"
          ])
    describe "parseStatement" $ do
        it  "hoge ;; huga ;; " $ parseStatement "hoge ;; huga ;; "
            `shouldBe` (Statement [V "hoge", V "huga"])
    -- fun x -> x+1
    -- fun x y -> x + y
