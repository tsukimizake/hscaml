{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module HsCaml.LLVM.Codegen where



import LLVM as L
import LLVM.AST as LA
import HsCaml.FrontEnd.Types
import Data.Text as T
import LLVM.Prelude as LP
import Data.ByteString.Short as B
import Text.Ascii

makeModule :: Text -> [Definition] -> LA.Module
makeModule name defs = LA.Module (B.pack . fmap ascii . T.unpack $ name) "main.ml" Nothing Nothing defs
