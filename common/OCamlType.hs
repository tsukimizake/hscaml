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

l *:  r = TInfixOpExpr l Mul r  (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l +:  r = TInfixOpExpr l Plus r  (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l -:  r = TInfixOpExpr l Minus r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l /:  r = TInfixOpExpr l Div r  (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l *.: r = TInfixOpExpr l MulDot r (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat)
l +.: r = TInfixOpExpr l PlusDot r (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat)
l -.: r = TInfixOpExpr l MinusDot r (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat)
l /.: r = TInfixOpExpr l DivDot r (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat)
l <:  r = TInfixOpExpr l (Compare LessThan) r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l <=: r = TInfixOpExpr l (Compare LessThanEq) r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l ==: r = TInfixOpExpr l (Compare Equal) r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l >:  r = TInfixOpExpr l (Compare GreaterThan) r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l >=: r = TInfixOpExpr l (Compare GreaterThanEq) r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
l &&: r = TInfixOpExpr l BoolAnd r (ocamlBool ::-> ocamlBool ::-> ocamlBool) 
l ||: r = TInfixOpExpr l BoolOr r (ocamlBool ::-> ocamlBool ::-> ocamlBool)
l %:  r = TInfixOpExpr l Mod r (ocamlInt ::-> ocamlInt ::-> ocamlInt)
