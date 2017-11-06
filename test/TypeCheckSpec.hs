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
                        (VarPattern UnspecifiedType (Sym "_x_gen_0"))
                        (IntC 1)
                        (LetIn (VarPattern UnspecifiedType (Sym "_x_gen_1"))
                         (IntC 2)
                         (LetIn (VarPattern UnspecifiedType (Sym "_x_gen_2"))
                          (IntC 3)
                          (V "_x_gen_2"))))

    describe "typecheck" $ do
        testTypeCheckExpr "let f x y = x*y in f"
            (TLetIn
             (FuncPattern
              (ocamlInt ::-> ocamlInt ::-> ocamlInt)
              (Sym "_f_gen_0") [Sym "_x_gen_0", Sym "_y_gen_0"])
             (TVar (Sym "_x_gen_0") ocamlInt :*: TVar (Sym "_y_gen_0") ocamlInt)
             (TVar (Sym "_f_gen_0") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
             (ocamlInt ::-> ocamlInt ::-> ocamlInt))
