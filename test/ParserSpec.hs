{-# LANGUAGE OverloadedStrings #-}
module ParserSpec(parserSpec) where
import Test.Hspec
import Data.Text
import Parser
import Types
import OCamlType
import Text.Parsec
import GHC.Exts

isRight (Right _) = True
isRight _ = False

instance IsString Sym where
    fromString x = Sym $ pack x

testParser :: String -> Expr -> Spec
testParser lhs rhs= it lhs $ do
        exprParser lhs `shouldBe` rhs

parserSpec :: Spec
parserSpec = do
    describe "parser" $ do
        testParser "82*3+3-(2-  2)+  (2  )  " ((((IntC 82) :* (IntC 3)) :+ (IntC 3)) :- (Paren ((IntC 2) :- (IntC 2))) :+ (Paren (IntC 2)))
        testParser "x*y-1" (((V "x") :* (V "y")) :- (IntC 1))
        testParser "x * f y -1" ((V "x" :* (FunApply "f" [V "y"])) :- (IntC 1))
        testParser "x - f y *1" (V "x" :- ((FunApply "f" [V "y"]) :* (IntC 1)))
        
        testParser "let eu = 4" (Let (VarPattern Nothing "eu") (Constant (IntVal 4)))
        testParser "let rec f x = x*x*x" (LetRec (FuncPattern Nothing "f" ["x"]) ((V "x" :* V "x") :* V "x"))
        testParser "let (eu:int) = 4" (Let (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) "eu")) (Constant (IntVal 4)))
        testParser "let rec eu = 4" (LetRec (VarPattern Nothing "eu") (Constant (IntVal 4)))
        testParser "let rec (eu : int  ) = 4"
            (LetRec (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) "eu")) (IntC 4))
        testParser "if x then y else z"
            (IfThenElse (V "x") (V "y") (V "z"))
        testParser "let rec fib x = if x<=1 then 1 else fib (x-1)+fib(x-2)"
            (LetRec (FuncPattern Nothing "fib" ["x"])
                    (IfThenElse ((V "x") :<= (IntC 1))
                     (IntC 1)
                     ((FunApply (Sym "fib") [(Paren $ V "x" :- IntC 1)])
                      :+
                      (FunApply (Sym "fib") [(Paren $ V "x" :- IntC 2)]))))
        testParser "let f a b = a = b"
            (Let (FuncPattern Nothing "f" [Sym "a", Sym "b"])
             ((V "a") :== (V "b")))
        testParser "type hoge = Hoge" (TypeDecl "hoge" [DataCnstr "Hoge" Nothing])
        testParser "type hoge = Hoge of hoge"
            (TypeDecl "hoge" [DataCnstr "Hoge" (Just (TypeAtom "hoge"))])
        testParser "type hoge = Hoge of hoge | Huga of hoge"
            (TypeDecl "hoge" [DataCnstr "Hoge" (Just (TypeAtom "hoge")),
                              DataCnstr "Huga" (Just (TypeAtom "hoge"))])
        testParser "type hoge = Hoge of hoge * hoge"
            (TypeDecl "hoge"
             [DataCnstr "Hoge" (Just ((TypeAtom "hoge") ::* (TypeAtom "hoge")))])
        testParser "type hoge = Hoge of hoge*hoge"
            (TypeDecl "hoge" [DataCnstr "Hoge" (Just ((TypeAtom "hoge") ::* (TypeAtom "hoge")))])
        testParser "type expr = Plus of expr * expr| Minus of expr * expr | Times of expr * expr | Divide of expr * expr | Value of string"
            (TypeDecl "expr"
             [DataCnstr "Plus" (Just $ (TypeAtom "expr") ::* (TypeAtom "expr")),
              DataCnstr "Minus" (Just $ (TypeAtom "expr") ::* (TypeAtom "expr")),
              DataCnstr "Times" (Just $ (TypeAtom "expr") ::* (TypeAtom "expr")),
              DataCnstr "Divide" (Just $ (TypeAtom "expr") ::* (TypeAtom "expr")),
              DataCnstr "Value" (Just $ TypeAtom "string")])
        testParser "let main = print_int 42"
            (Let (VarPattern Nothing (Sym "main")) (FunApply (Sym "print_int") [(IntC 42)]))
        -- testParser "match "
