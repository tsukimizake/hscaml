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

pattern (:*:) :: TExpr -> TExpr -> TExpr
pattern l :*: r = TInfixOpExpr l Mul r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:+:) :: TExpr -> TExpr -> TExpr
pattern l :+: r = TInfixOpExpr l Plus r  (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:-:) :: TExpr -> TExpr -> TExpr
pattern l :-: r = TInfixOpExpr l Minus r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:/:) :: TExpr -> TExpr -> TExpr
pattern l :/: r = TInfixOpExpr l Div r  (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:*.:) :: TExpr -> TExpr -> TExpr
pattern l :*.: r = TInfixOpExpr l MulDot r (TypeAtom "float" ::-> TypeAtom "float" ::-> TypeAtom "float")
pattern (:+.:) :: TExpr -> TExpr -> TExpr
pattern l :+.: r = TInfixOpExpr l PlusDot r (TypeAtom "float" ::-> TypeAtom "float" ::-> TypeAtom "float")
pattern (:-.:) :: TExpr -> TExpr -> TExpr
pattern l :-.: r = TInfixOpExpr l MinusDot r (TypeAtom "float" ::-> TypeAtom "float" ::-> TypeAtom "float")
pattern (:/.:) :: TExpr -> TExpr -> TExpr
pattern l :/.: r = TInfixOpExpr l DivDot r (TypeAtom "float" ::-> TypeAtom "float" ::-> TypeAtom "float")
pattern (:<:) :: TExpr -> TExpr -> TExpr
pattern l :<: r = TInfixOpExpr l (Compare LessThan) r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:<=:) :: TExpr -> TExpr -> TExpr
pattern l :<=: r = TInfixOpExpr l (Compare LessThanEq) r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:==:) :: TExpr -> TExpr -> TExpr
pattern l :==: r = TInfixOpExpr l (Compare Equal) r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:>:) :: TExpr -> TExpr -> TExpr
pattern l :>: r = TInfixOpExpr l (Compare GreaterThan) r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:>=:) :: TExpr -> TExpr -> TExpr
pattern l :>=: r = TInfixOpExpr l (Compare GreaterThanEq) r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
pattern (:&&:) :: TExpr -> TExpr -> TExpr
pattern l :&&: r = TInfixOpExpr l BoolAnd r (TypeAtom "bool" ::-> TypeAtom "bool" ::-> TypeAtom "bool") 
pattern (:||:) :: TExpr -> TExpr -> TExpr
pattern l :||: r = TInfixOpExpr l BoolOr r (TypeAtom "bool" ::-> TypeAtom "bool" ::-> TypeAtom "bool")
pattern (:%:) :: TExpr -> TExpr -> TExpr
pattern l :%: r = TInfixOpExpr l Mod r (TypeAtom "int" ::-> TypeAtom "int" ::-> TypeAtom "int")
