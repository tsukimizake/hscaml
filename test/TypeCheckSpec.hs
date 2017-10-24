{-# LANGUAGE OverloadedStrings #-}
module TypeCheckSpec where
import Test.Hspec
import Types
import OCamlType
import Parser
import TypeChecker
import Data.Text

testTypeCheck :: String -> TExpr -> Spec
testTypeCheck src ast = it src $ do
    (typeCheck . exprParser $ src) `shouldBe` ast

typeCheckSpec :: Spec
typeCheckSpec = do
    describe "typecheck" $ do
        testTypeCheck "let f x y = x*y in f"
            (TLetIn
             (FuncPattern
              (Just $ ocamlInt ::-> ocamlInt ::-> ocamlInt)
              (Sym "f") [Sym "x", Sym "y"])
             (TVar (Sym "x") ocamlInt :*: TVar (Sym "y") ocamlInt)
             (TVar (Sym "f") (ocamlInt ::-> ocamlInt ::-> ocamlInt))
             (ocamlInt ::-> ocamlInt ::-> ocamlInt))
            -- (LetIn
            --   (FuncPattern
            --    Nothing
            --    (Sym "f") [Sym "x", Sym "y"])
            --   ((V "x") :* (V "y"))
            --   (V "f"))
