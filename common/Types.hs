{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

type Name = Text

data Sym = Sym {
    _symname :: Name
} deriving(Show, Eq)


data Value = IntVal Int deriving(Show, Eq)

data TypeExpr = TypeAtom Text
              | TypeExpr ::-> TypeExpr
              | TypeExpr ::* TypeExpr
              | TypeExpr ::+ TypeExpr deriving(Show, Eq)

-- typename :: Prism' TypeExpr Text
-- typename = prism' (\name -> TypeAtom name) getter
--   where
--     getter (TypeAtom name) = Just name
--     getter _ = Nothing

-- typefrom, typeto, typeleft, typeright :: Prism' TypeExpr TypeExpr
-- typefrom = prism' wrap unwrap
--   where
--     wrap :: TypeExpr -> TypeExpr
--     wrap l = l
--     unwrap :: TypeExpr -> Maybe TypeExpr
--     unwrap (l :-> r) = Just l
--     unwrap _ = Nothing

-- typeto = prism' wrap unwrap
--   where
--     wrap = id
--     unwrap (l :-> r) = Just r
--     unwrap _ = Nothing


-- typeleft = prism' wrap unwrap
--   where
--     wrap = id
--     unwrap (l :* r) = Just l
--     unwrap _ = Nothing


-- typeright = prism' wrap unwrap
--   where
--     wrap = id
--     unwrap (l :* r) = Just r
--     unwrap _ = Nothing

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
          | Function PatternMatching
          | Fun PatternMatching
          | Let Pattern Expr
          | LetRec Pattern Expr
          deriving(Show, Eq)

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
     _left :: Pattern,
     _right :: Pattern
   } deriving (Show, Eq)

data Comp = LessThan | LessThanEq | Equal | GreaterThan | GreaterThanEq deriving (Show, Eq)
data InfixOp = InfixSymbol
             | Mul | Plus | Minus | Div
             | MulDot | PlusDot | MinusDot | DivDot
             | Compare Comp
             | BoolAnd
             | BoolOr
             | BinAnd
             | BinOr
             | Mod
             deriving(Show, Eq)

data PatternMatching = PatternMatching Pattern Expr deriving(Show, Eq)

makeLenses ''Sym
makeFields ''TypedExpr
makeLenses ''Pattern
-- makePrisms ''TypeExpr
-- makePrisms ''Expr
-- makePrisms ''Comp
-- makePrisms ''InfixOp
-- makePrisms ''Pattern
-- makePrisms ''LetBinding
