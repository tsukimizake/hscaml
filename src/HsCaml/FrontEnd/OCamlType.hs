{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall #-}

module HsCaml.FrontEnd.OCamlType where

import HsCaml.FrontEnd.Types

ocamlInt :: TypeExpr
ocamlInt = TypeAtom "int"

ocamlChar :: TypeExpr
ocamlChar = TypeAtom "char"

ocamlBool :: TypeExpr
ocamlBool = TypeAtom "bool"

ocamlFloat :: TypeExpr
ocamlFloat = TypeAtom "float"

ocamlUnit :: TypeExpr
ocamlUnit = TypeAtom "()"
