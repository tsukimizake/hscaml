{-# LANGUAGE OverloadedStrings #-}
module OCamlType where
import Types

ocamlInt :: TypeExpr
ocamlInt = TypeAtom "Int"

ocamlChar :: TypeExpr
ocamlChar = TypeAtom "Char"
