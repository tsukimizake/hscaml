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
        
        testParser "let eu = 4" (Let (VarPattern Nothing "eu") (Constant (IntVal 4)))
        testParser "let rec f x = x*x*x" (LetRec (FuncPattern Nothing "f" ["x"]) ((V "x" :* V "x") :* V "x"))
        testParser "let (eu:Int) = 4" (Let (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) "eu")) (Constant (IntVal 4)))
        testParser "let rec eu = 4" (LetRec (VarPattern Nothing "eu") (Constant (IntVal 4)))
        testParser "let rec (eu : Int  ) = 4"
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
