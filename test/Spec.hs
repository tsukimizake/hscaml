{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Data.Text
import Data.Set as S
import HsCaml.FrontEnd.Types
import HsCaml.TypeChecker.TypeCheckUtil
import HsCaml.TypeChecker.RenameSymsByScope
import HsCaml.TypeChecker.InitialTypeInfer
import HsCaml.TypeChecker.CollectTypeConstraints
import HsCaml.TypeChecker.TypeChecker
import HsCaml.Parser.Parser
import ParserSpec
import TypeCheckSpec
import HCcoreSpec
import Control.Monad.State

main :: IO ()
main = do
    hspec parserSpec
    hspec typeCheckSpec
    hspec hcCoreSpec
