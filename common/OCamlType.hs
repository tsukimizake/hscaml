{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module OCamlType where
import Types

ocamlInt :: TypeExpr
ocamlInt = TypeAtom "int"

ocamlChar :: TypeExpr
ocamlChar = TypeAtom "char"

ocamlBool :: TypeExpr
ocamlBool = TypeAtom "bool"

ocamlFloat :: TypeExpr
ocamlFloat = TypeAtom "float"

