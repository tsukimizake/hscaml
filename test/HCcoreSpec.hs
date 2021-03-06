{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module HCcoreSpec where

import HsCaml.FrontEnd.Types
import HsCaml.HCcore.ToHCcore
import HsCaml.HCcore.Types
import HsCaml.Parser.Parser
import HsCaml.TypeChecker.TypeChecker
import Test.Hspec

testToHCcore :: String -> CExpr -> TypeEnv -> Spec
testToHCcore src expr env = it src $ do
  let cexpr = flip toHCcore env =<< (typeCheck . parseExpr $ src)
  case cexpr of
    Left err -> error $ show err
    Right res -> res `shouldBe` expr

hcCoreSpec :: Spec
hcCoreSpec = describe "toHCcore" $ do
  testToHCcore
    "let a = 1"
    ( CMultiExpr
        [ CLetRec
            ( LetPattern
                (TypeAtom "int")
                (VarPattern (TypeAtom "int") "_a_gen_0")
            )
            (CValue (CLConst (IntVal 1) (TypeAtom "int")) (TypeAtom "int"))
            (TypeAtom "()")
        ]
        (TypeAtom "()")
    )
    (TypeEnv [])
  pure ()
