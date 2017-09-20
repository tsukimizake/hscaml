{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Parser
import Types
import OCamlType
import Text.Parsec

isRight (Right _) = True
isRight _ = False

specParser :: Spec
specParser = do
    describe "parser" $ do
        it "parse let" $ do
            (parse exprParser "" "let eu = 4") `shouldBe` Right (Let (VarPattern Nothing (Sym "eu")) (Constant (IntVal 4)))
            (parse exprParser "" "let f x = x*x*x") `shouldBe` Right (Let (FuncPattern Nothing (Sym "f") [Sym "x"]) (InfixOpExpr (InfixOpExpr (Var (Sym "x")) Mul (Var (Sym "x"))) Mul (Var (Sym "x"))))
            (parse exprParser "" "let eu:Int = 4") `shouldBe` (Right (Let (VarPattern Nothing (Sym  "eu")) (Constant (IntVal 4))))
        it "parse letrec" $ do
            (parse exprParser "" "let rec eu = 4") `shouldBe` (Right (LetRec (VarPattern Nothing (Sym "eu")) (Constant (IntVal 4))))
            (parse exprParser "" "let rec (eu : Int) = 4") `shouldBe` (Right (LetRec (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) (Sym "eu"))) (Constant (IntVal 4))))
        it "parse mathExpr" $ do (parse exprParser "" "82*3+3-(2-  2)+(2)") `shouldBe` Right (InfixOpExpr (InfixOpExpr (InfixOpExpr (InfixOpExpr (Constant (IntVal 82)) Mul (Constant (IntVal 3))) Plus (Constant (IntVal 3))) Minus (Paren (InfixOpExpr (Constant (IntVal 2)) Minus (Constant (IntVal 2))))) Plus (Paren (Constant (IntVal 2))))
            
main :: IO ()
main = do
    hspec specParser

