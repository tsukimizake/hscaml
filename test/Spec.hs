{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Data.Text
import Parser
import Types
import OCamlType
import TypeCheckUtil
import InitialTypeInfer
import RenameSymsByScope
import CollectTypeConstraints
import TypeChecker
import ParserSpec
import TypeCheckSpec
import Data.Set as S
import Control.Monad.State

specExpr :: String -> Expr -> TExpr -> [TypeConstraint] -> Spec
specExpr input parsed initialTypeInfered constraints = it input $ do
    let expr = renameSymsByScope . parseExpr $ input
    expr `shouldBe` parsed
    let texpr = initialTypeInfer expr
    texpr `shouldBe` initialTypeInfered
    let cs = collectTypeConstraints texpr
    -- traceM $ show texpr
    cs `shouldBe` S.fromList constraints

main :: IO ()
main = do
    hspec parserSpec
    hspec typeCheckSpec
