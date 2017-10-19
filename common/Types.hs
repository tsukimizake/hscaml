{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Types where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

type Name = Text

data Sym = Sym {
    _Symname :: Name
} deriving(Show, Eq)


data Value = IntVal Int deriving(Show, Eq)

data TypeExpr = TypeAtom Text
              | TypeExpr ::-> TypeExpr
              | TypeExpr ::* TypeExpr
              | TypeExpr ::+ TypeExpr
              deriving(Show, Eq)

data DataCnstr = DataCnstr Name (Maybe TypeExpr)
                 deriving (Show, Eq)

data Expr = Constant Value
          | Var Sym
          | Paren Expr
          | InfixOpExpr Expr InfixOp Expr
          | BegEnd Expr
          | ExprWithType Expr TypeExpr
          | MultiExpr [Expr]
          | Constr Expr
          | IfThenElse Expr Expr Expr
          | Match Expr PatternMatching
          | While Expr Expr
          | FunApply Sym [Expr]
          | Let Pattern Expr
          | LetRec Pattern Expr
          | LetIn Pattern Expr Expr
          | TypeDecl Name [DataCnstr]
          deriving(Show, Eq)

pattern IntC x = Constant (IntVal x)
pattern V x = Var (Sym x)
pattern l :*  r = InfixOpExpr l Mul r
pattern l :+  r = InfixOpExpr l Plus r
pattern l :-  r = InfixOpExpr l Minus r
pattern l :/  r = InfixOpExpr l Div r
pattern l :*. r = InfixOpExpr l MulDot r
pattern l :+. r = InfixOpExpr l PlusDot r
pattern l :-. r = InfixOpExpr l MinusDot r
pattern l :/. r = InfixOpExpr l DivDot r
pattern l :<  r = InfixOpExpr l (Compare LessThan) r
pattern l :<= r = InfixOpExpr l (Compare LessThanEq) r
pattern l :== r = InfixOpExpr l (Compare Equal) r
pattern l :>  r = InfixOpExpr l (Compare GreaterThan) r
pattern l :>= r = InfixOpExpr l (Compare GreaterThanEq) r
pattern l :&& r = InfixOpExpr l BoolAnd r
pattern l :|| r = InfixOpExpr l BoolOr r
pattern l :%  r = InfixOpExpr l Mod r

data TypedExpr = TypedExpr{
    _TypedExprtypeExpr :: TypeExpr,
    _TypedExprbodyExpr :: Expr
} deriving (Show, Eq)

data Pattern = VarPattern {
    _patType :: (Maybe TypeExpr),
    _sym :: Sym
}| ConstantPattern {
    _patType :: (Maybe TypeExpr), 
    _val :: Value
}| ParenPattern {
     _patType :: (Maybe TypeExpr),
     _pat :: Pattern
}| ListPattern {
     _patType :: (Maybe TypeExpr), 
     _exp :: [Expr]
}| FuncPattern {
    _patType :: (Maybe TypeExpr),
    _sym :: Sym,
    _args :: [Sym]
}| OrPattern{
    _patType :: (Maybe TypeExpr),
    _left :: Pattern,
    _right :: Pattern
} deriving (Show, Eq)

data Comp = LessThan | LessThanEq | Equal | GreaterThan | GreaterThanEq deriving (Show, Eq)
data InfixOp = Mul | Plus | Minus | Div
             | MulDot | PlusDot | MinusDot | DivDot
             | Compare Comp
             | BoolAnd
             | BoolOr
             | Mod
             deriving(Show, Eq)

data PatternMatching = PatternMatching Pattern Expr deriving(Show, Eq)

makeFields ''Sym
makeFields ''TypedExpr
makeLenses ''Pattern
-- makePrisms ''TypeExpr
-- makePrisms ''Expr
-- makePrisms ''Comp
-- makePrisms ''InfixOp
-- makePrisms ''Pattern
-- makePrisms ''LetBinding
