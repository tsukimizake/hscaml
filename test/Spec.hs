{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Data.Text
import Parser
import Types
import OCamlType
import Text.Parsec
import GHC.Exts
import ParserSpec

main :: IO ()
main = do
    hspec parserSpec
