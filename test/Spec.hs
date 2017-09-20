{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Data.Text
import Parser
import Types
import OCamlType
import Text.Parsec

isRight (Right _) = True
isRight _ = False

parseTest :: String -> Either ParseError Expr -> Spec
parseTest lhs rhs= it lhs $ do
        (parse exprParser "" (pack lhs)) `shouldBe` rhs

specParser :: Spec
specParser = do
    describe "parser" $ do
        Main.parseTest "82*3+3-(2-  2)+(2)" (Right (InfixOpExpr (InfixOpExpr (InfixOpExpr (InfixOpExpr (Constant (IntVal 82)) Mul (Constant (IntVal 3))) Plus (Constant (IntVal 3))) Minus (Paren (InfixOpExpr (Constant (IntVal 2)) Minus (Constant (IntVal 2))))) Plus (Paren (Constant (IntVal 2)))))
        
        Main.parseTest "x*y-1" (Right (InfixOpExpr (InfixOpExpr (Var (Sym "x")) Mul (Var (Sym "y"))) Minus (Constant (IntVal 1))))
        Main.parseTest "x * f y -1" (Right (InfixOpExpr (InfixOpExpr (Var (Sym "x")) Mul (FunApply (Sym "f") [Var $ Sym "y"])) Minus (Constant (IntVal 1))))
        
        Main.parseTest "let eu = 4" (Right (Let (VarPattern Nothing (Sym "eu")) (Constant (IntVal 4))))
        Main.parseTest "let f x = x*x*x" (Right (Let (FuncPattern Nothing (Sym "f") [Sym "x"]) (InfixOpExpr (InfixOpExpr (Var (Sym "x")) Mul (Var (Sym "x"))) Mul (Var (Sym "x")))))
        Main.parseTest "let eu:Int = 4" ((Right (Let (VarPattern Nothing (Sym  "eu")) (Constant (IntVal 4)))))
        Main.parseTest "let rec eu = 4" ((Right (LetRec (VarPattern Nothing (Sym "eu")) (Constant (IntVal 4)))))
        Main.parseTest "let rec (eu : Int) = 4" ((Right (LetRec (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) (Sym "eu"))) (Constant (IntVal 4)))))
            
main :: IO ()
main = do
    hspec specParser

