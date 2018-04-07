{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module HsCaml.Common.OCamlType where
import HsCaml.Common.Types

ocamlInt :: TypeExpr
ocamlInt = TypeAtom "int"

ocamlChar :: TypeExpr
ocamlChar = TypeAtom "char"

ocamlBool :: TypeExpr
ocamlBool = TypeAtom "bool"

ocamlFloat :: TypeExpr
ocamlFloat = TypeAtom "float"
