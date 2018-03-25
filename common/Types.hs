{-# OPTIONS -Wall #-}
{-# LANGUAGE InstanceSigs, TemplateHaskell, PatternSynonyms, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, OverloadedStrings #-}
module Types where

import Control.Lens
import Data.Text (Text)

type Name = Text

data Sym = Sym {
    __name :: Name
} deriving(Show, Eq, Ord)

data Value = IntVal Int
           | BoolVal Bool
             deriving(Show, Eq, Ord)

-- 型式
data TypeExpr = TypeAtom Text
              | TypeExpr ::-> TypeExpr
              | TypeExpr ::* TypeExpr
              | TypeExpr ::+ TypeExpr
              | ParenTypeExpr TypeExpr
              | UnspecifiedType
              | TypeVar Text
              | TypeApplication [TypeExpr] TypeExpr
             deriving(Show, Eq, Ord)

infixr ::->

data CompileError = TypeError Text
                  | ParseError Text -- TODO
                  deriving (Show)

data DataCnstr = DataCnstr Name [TypeExpr]
                 deriving (Show, Eq, Ord)


-- 式
data Expr = Constant Value
          | Var Sym
          | Paren Expr
          | InfixOpExpr Expr InfixOp Expr
          | BegEnd Expr
          | MultiExpr [Expr]
          | Constr Expr
          | IfThenElse Expr Expr Expr
          | Match Expr [(Pattern, Expr)]
          | While Expr Expr
          | FunApply Expr [Expr]
          | Let Pattern Expr
          | LetRec Pattern Expr
          | LetIn Pattern Expr Expr
          | TypeDecl Name [DataCnstr]
          deriving(Show, Eq, Ord)

-- 型付いた式
data TExpr = TConstant Value TypeExpr
           | TVar Sym TypeExpr
           | TParen TExpr TypeExpr
           | TInfixOpExpr TExpr InfixOp TExpr TypeExpr
           | TBegEnd TExpr TypeExpr
           | TMultiExpr [TExpr] TypeExpr
           | TConstr TExpr TypeExpr
           | TIfThenElse TExpr TExpr TExpr TypeExpr
           | TMatch TExpr [(Pattern, TExpr)] TypeExpr
           | TWhile TExpr TExpr TypeExpr
           | TFunApply TExpr [TExpr] TypeExpr
           | TLet Pattern TExpr TypeExpr
           | TLetRec Pattern TExpr TypeExpr
           | TLetIn Pattern TExpr TExpr TypeExpr
           | TTypeDecl Name [DataCnstr] TypeExpr
           deriving(Show, Eq, Ord)

pattern IntC :: Int -> Expr
pattern IntC x = Constant (IntVal x)
pattern BoolC :: Bool -> Expr
pattern BoolC x = Constant (BoolVal x)
pattern V :: Text -> Expr
pattern V x = Var (Sym x)
pattern (:*) :: Expr -> Expr -> Expr
pattern l :*  r = InfixOpExpr l Mul r
pattern (:+) :: Expr -> Expr -> Expr
pattern l :+  r = InfixOpExpr l Plus r
pattern (:-) :: Expr -> Expr -> Expr
pattern l :-  r = InfixOpExpr l Minus r
pattern (:/) :: Expr -> Expr -> Expr
pattern l :/  r = InfixOpExpr l Div r
pattern (:*.) :: Expr -> Expr -> Expr
pattern l :*. r = InfixOpExpr l MulDot r
pattern (:+.) :: Expr -> Expr -> Expr
pattern l :+. r = InfixOpExpr l PlusDot r
pattern (:-.) :: Expr -> Expr -> Expr
pattern l :-. r = InfixOpExpr l MinusDot r
pattern (:/.) :: Expr -> Expr -> Expr
pattern l :/. r = InfixOpExpr l DivDot r
pattern (:<) :: Expr -> Expr -> Expr
pattern l :<  r = InfixOpExpr l (Compare LessThan) r
pattern (:<=) :: Expr -> Expr -> Expr
pattern l :<= r = InfixOpExpr l (Compare LessThanEq) r
pattern (:==) :: Expr -> Expr -> Expr
pattern l :== r = InfixOpExpr l (Compare Equal) r
pattern (:>) :: Expr -> Expr -> Expr
pattern l :>  r = InfixOpExpr l (Compare GreaterThan) r
pattern (:>=) :: Expr -> Expr -> Expr
pattern l :>= r = InfixOpExpr l (Compare GreaterThanEq) r
pattern (:&&) :: Expr -> Expr -> Expr
pattern l :&& r = InfixOpExpr l BoolAnd r
pattern (:||) :: Expr -> Expr -> Expr
pattern l :|| r = InfixOpExpr l BoolOr r
pattern (:%) :: Expr -> Expr -> Expr
pattern l :%  r = InfixOpExpr l Mod r

pattern TIntC :: Int -> TExpr
pattern TIntC x = TConstant (IntVal x) (TypeAtom "Bool")
pattern TBoolC :: Bool -> TExpr
pattern TBoolC x = TConstant (BoolVal x) (TypeAtom "Bool")
pattern TV :: Text -> TExpr
pattern TV x = TVar (Sym x) UnspecifiedType
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

data Statement = Statement [Expr] deriving (Show, Eq)
data TStatement = TStatement [TExpr] deriving (Show, Eq)

data Pattern = VarPattern {
    __patType :: TypeExpr,
    __sym :: Sym
}| ConstantPattern {
    __patType :: TypeExpr,
    __val :: Value
}| ParenPattern {
    __patType :: TypeExpr,
    __pat :: Pattern
}| ListPattern {
    __patType :: TypeExpr,
    __exprs :: [Expr]
}| FuncPattern {
    __patType :: TypeExpr,
    __sym :: Sym,
    __args :: [(Sym, TypeExpr)]
}| OrPattern{
    __patType :: TypeExpr,
    __left :: Pattern,
    __right :: Pattern
} deriving (Show, Eq, Ord)

data Comp = LessThan | LessThanEq | Equal | GreaterThan | GreaterThanEq deriving (Show, Eq, Ord)
data InfixOp = Mul | Plus | Minus | Div
             | MulDot | PlusDot | MinusDot | DivDot
             | Compare Comp
             | BoolAnd
             | BoolOr
             | Mod
             deriving (Show, Eq, Ord)


makeLenses ''Sym
makePrisms ''Expr
makeClassyFor "HasTypeExpr" "_typeExpr" [] ''TypeExpr
makeLenses ''Pattern
makeLenses ''TExpr
makeClassyPrisms ''TExpr

instance HasTypeExpr TExpr where
    _typeExpr :: Lens' TExpr TypeExpr
    _typeExpr = lens getter setter
      where
        getter :: TExpr -> TypeExpr
        getter (TConstant _ x)= x
        getter (TVar _ x) = x
        getter (TParen _ x) = x
        getter (TInfixOpExpr _ _ _ x) = x
        getter (TBegEnd _ x) = x
        getter (TMultiExpr _ x) = x
        getter (TConstr _ x) = x
        getter (TIfThenElse _ _ _ x) = x
        getter (TMatch _ _ x) = x
        getter (TWhile _ _ x) = x
        getter (TFunApply _ _ x) = x
        getter (TLet _ _ x) = x
        getter (TLetRec _ _ x) = x
        getter (TLetIn _ _ _ x) = x
        getter (TTypeDecl _ _ x) = x
        setter :: TExpr -> TypeExpr -> TExpr
        setter (TConstant e _) x = TConstant e x
        setter (TVar e _) x = TVar e x
        setter (TParen e _) x = TParen e x
        setter (TInfixOpExpr e f g _) x = TInfixOpExpr e f g x
        setter (TBegEnd e _) x = TBegEnd e x
        setter (TMultiExpr e _) x = TMultiExpr e x
        setter (TConstr e _) x = TConstr e x
        setter (TIfThenElse e f g _) x = TIfThenElse e f g x
        setter (TMatch e f _) x = TMatch e f x
        setter (TWhile e f _) x = TWhile e f x
        setter (TFunApply e f _) x = TFunApply e f x
        setter (TLet e f _) x = TLet e f x
        setter (TLetRec e f _) x = TLetRec e f x
        setter (TLetIn e f g _) x = TLetIn e f g x
        setter (TTypeDecl e f _) x = TTypeDecl e f x

instance AsTExpr Expr where
    _TExpr  = prism toExpr toTExpr
      where
        toExpr :: TExpr -> Expr
        toExpr (TConstant x _)= Constant x
        toExpr (TVar x _) = Var x
        toExpr (TParen x _) = Paren (toExpr x)
        toExpr (TInfixOpExpr x y z _) = InfixOpExpr (toExpr x) y (toExpr z)
        toExpr (TBegEnd x _) = BegEnd (toExpr x)
        toExpr (TMultiExpr x _) = MultiExpr (fmap toExpr x)
        toExpr (TConstr x _) = Constr (toExpr x)
        toExpr (TIfThenElse x y z _) = IfThenElse (toExpr x) (toExpr y) (toExpr z)
        toExpr (TMatch x y _) = Match (toExpr x) (fmap toExpr <$> y)
        toExpr (TWhile x y _) = While (toExpr x) (toExpr y)
        toExpr (TFunApply x y _) = FunApply (toExpr x) (fmap toExpr y)
        toExpr (TLet x y _) = Let x (toExpr y)
        toExpr (TLetRec x y _) = LetRec x (toExpr y)
        toExpr (TLetIn x y z _) = LetIn x (toExpr y) (toExpr z)
        toExpr (TTypeDecl x y _) = TypeDecl x y
        toTExpr :: Expr -> Either Expr TExpr
        toTExpr e = Left e
