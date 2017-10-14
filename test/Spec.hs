{-# LANGUAGE OverloadedStrings #-}
module Main where
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

parseTest :: String -> Either ParseError Expr -> Spec
parseTest lhs rhs= it lhs $ do
        (parse exprParser "" (pack lhs)) `shouldBe` rhs

specParser :: Spec
specParser = do
    describe "parser" $ do
        Main.parseTest "82*3+3-(2-  2)+  (2  )  " (Right ((((IntC 82) :* (IntC 3)) :+ (IntC 3)) :- (Paren ((IntC 2) :- (IntC 2))) :+ (Paren (IntC 2))))
        Main.parseTest "x*y-1" (Right (((V "x") :* (V "y")) :- (IntC 1)))
        Main.parseTest "x * f y -1" (Right ((V "x" :* (FunApply "f" [V "y"])) :- (IntC 1)))
        
        Main.parseTest "let eu = 4" (Right (Let (VarPattern Nothing "eu") (Constant (IntVal 4))))
        Main.parseTest "let rec f x = x*x*x" (Right (LetRec (FuncPattern Nothing "f" ["x"]) ((V "x" :* V "x") :* V "x")))
        Main.parseTest "let (eu:Int) = 4" (Right (Let (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) "eu")) (Constant (IntVal 4))))
        Main.parseTest "let rec eu = 4" (Right (LetRec (VarPattern Nothing "eu") (Constant (IntVal 4))))
        Main.parseTest "let rec (eu : Int  ) = 4" ((Right (LetRec (ParenPattern (Just ocamlInt) (VarPattern (Just ocamlInt) "eu")) (IntC 4))))

        Main.parseTest "if x then y else z"
            (Right $ IfThenElse (V "x") (V "y") (V "z"))
        Main.parseTest "let rec fib x = if x<=1 then 1 else fib (x-1)+fib(x-2)"
            (Right (LetRec (FuncPattern Nothing "fib" ["x"])
                    (IfThenElse ((V "x") :<= (IntC 1))
                     (IntC 1)
                     ((FunApply (Sym "fib") [V "x" :- IntC 1])
                      :+
                      (FunApply (Sym "fib") [V "x"] :- IntC 2)))))

main :: IO ()
main = do
    hspec specParser
