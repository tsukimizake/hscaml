{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Data.Text
import Data.Set as S
import HsCaml.Common.Types
import HsCaml.TypeChecker.TypeCheckUtil
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.InitialTypeInfer
import HsCaml.TypeChecker.CollectTypeConstraints
import HsCaml.TypeChecker.TypeChecker
import HsCaml.Parser.Parser
import ParserSpec
import TypeCheckSpec
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
