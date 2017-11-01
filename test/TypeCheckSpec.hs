{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheckSpec where
import Test.Hspec
import Types
import OCamlType
import Parser
import TypeChecker

testTypeCheckExpr :: String -> TExpr -> Spec
testTypeCheckExpr src ast = it src $ do
    let res = typeCheck . parseExpr $ src
    case res of
        Left err -> error $ show err
        Right res' -> res' `shouldBe` ast

typeCheckSpec :: Spec
typeCheckSpec = do
    describe "renameSymsByScope" $ it "let x = 1 in let x = 2 in let x = 3 in x" $ do
        (renameSymsByScope . parseExpr $ "let x = 1 in let x = 2 in let x = 3 in x")
            `shouldBe` (LetIn
                        (VarPattern Nothing (Sym "x_gen_0"))
                        (IntC 1)
                        (LetIn (VarPattern Nothing (Sym "x_gen_1"))
                         (IntC 2)
                         (LetIn (VarPattern Nothing (Sym "x_gen_2"))
                          (IntC 3)
                          (V "x_gen_2"))))

    describe "typecheck" $ do
        testTypeCheckExpr "let f x y = x*y in f"
            (TLetIn
             (FuncPattern
              (Just $ ocamlInt ::-> ocamlInt ::-> ocamlInt)
              (Sym "f_gen_0") [Sym "x_gen_0", Sym "y_gen_0"])
             (TVar (Sym "x_gen_0") ocamlInt :*: TVar (Sym "y_gen_0") ocamlInt)
             (TVar (Sym "f_gen_0") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
             (ocamlInt ::-> ocamlInt ::-> ocamlInt))
