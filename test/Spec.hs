{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Data.Text
import Parser
import Types
import OCamlType
import GHC.Exts
import ParserSpec
import TypeCheckSpec

main :: IO ()
main = do
    hspec parserSpec
    hspec typeCheckSpec
