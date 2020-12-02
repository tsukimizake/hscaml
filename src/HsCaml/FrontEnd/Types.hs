{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HsCaml.FrontEnd.Types where

import qualified Control.Lens as L
import Data.Text (Text)
import Deriving.Show.Simple
import GHC.Generics

type Name = Text

newtype Sym = Sym
  { __name :: Name
  }
  deriving (Eq, Ord, Generic)
  deriving (Show) via (WrapSimple Sym)

data Value
  = IntVal Int
  | BoolVal Bool
  deriving (Show, Eq, Ord)

type Level = Int

-- 型式
data TypeExpr
  = TypeAtom Text
  | TypeExpr ::-> TypeExpr
  | TypeExpr ::* TypeExpr
  | TypeExpr ::+ TypeExpr
  | ParenTypeExpr TypeExpr
  | UnspecifiedType
  | TypeVar Text Level
  | QVar Text -- forall qualified type var
  | TypeApplication [TypeExpr] TypeExpr
  deriving (Show, Eq, Ord)

infixr 9 ::->

data CompileError
  = TypeError Text
  | ParseError Text -- TODO
  | SemanticsError Text
  deriving (Show)

data DataCnstr = DataCnstr Name [TypeExpr]
  deriving (Show, Eq, Ord)

data TypeDecl = TypeDecl Name [DataCnstr]
  deriving (Show, Eq)

newtype TypeEnv = TypeEnv [TypeDecl]
  deriving (Show)

-- 式
data Expr
  = Constant Value
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
  | Let LetPattern Expr
  | LetRec LetPattern Expr
  | LetRecIn LetPattern Expr Expr
  | LetIn LetPattern Expr Expr
  | List [Expr]
  | Array [Expr]
  deriving (Show, Eq, Ord)

-- 型付いた式
data TExpr
  = TConstant Value TypeExpr
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
  | TLet LetPattern TExpr TypeExpr
  | TLetRec LetPattern TExpr TypeExpr
  | TLetIn LetPattern TExpr TExpr TypeExpr
  | TLetRecIn LetPattern TExpr TExpr TypeExpr
  | TList [TExpr] TypeExpr
  | TArray [TExpr] TypeExpr
  deriving (Show, Eq, Ord)

data TopLevel
  = TopLevelExpr Expr
  | TopLevelTypeDecl TypeDecl
  deriving (Show, Eq)

newtype Statement = Statement [TopLevel] deriving (Show, Eq)

data TStatement = TStatement [TExpr] [TypeDecl] deriving (Show, Eq)

data LetPattern
  = FuncLetPattern
      { _lpatType_ :: TypeExpr,
        _funcsym_ :: Sym,
        _args_ :: [(Sym, TypeExpr)]
      }
  | LetPattern
      { _lpatType_ :: TypeExpr,
        _pat_ :: Pattern
      }
  deriving (Eq, Ord, Generic)
  deriving (Show) via (WrapSimple LetPattern)

data Pattern
  = ConstantPattern
      { _mpatType_ :: TypeExpr,
        _val_ :: Value
      }
  | ParenPattern
      { _mpatType_ :: TypeExpr,
        _mpat_ :: Pattern
      }
  | ListPattern
      { _mpatType_ :: TypeExpr,
        _patterns_ :: [Pattern]
      }
  | OrPattern
      { _mpatType_ :: TypeExpr,
        _left_ :: Pattern,
        _right_ :: Pattern
      }
  | VarPattern
      { _mpatType_ :: TypeExpr,
        _sym_ :: Sym
      }
  | ConstrPattern
      { _mpatType_ :: TypeExpr,
        _constr_ :: Sym,
        _rest_ :: Pattern,
        _dcId_ :: Maybe Int
      }
  deriving (Eq, Ord, Generic)
  deriving (Show) via (WrapSimple Pattern)

data Comp = LessThan | LessThanEq | Equal | GreaterThan | GreaterThanEq deriving (Show, Eq, Ord)

data InfixOp
  = Mul
  | Plus
  | Minus
  | Div
  | MulDot
  | PlusDot
  | MinusDot
  | DivDot
  | Compare Comp
  | BoolAnd
  | BoolOr
  | Mod
  deriving (Show, Eq, Ord)

L.makeLenses ''Sym
L.makePrisms ''Expr
L.makeClassyFor "HasTypeExpr" "typeExpr_" [] ''TypeExpr
L.makeClassyFor "HasPatType" "patType_" [] ''TypeExpr
L.makeLenses ''LetPattern
L.makeLenses ''Pattern
L.makeLenses ''TExpr
L.makeClassyPrisms ''TExpr

instance HasPatType LetPattern where
  patType_ = lpatType_

instance HasPatType Pattern where
  patType_ = mpatType_

instance HasTypeExpr TExpr where
  typeExpr_ :: L.Lens' TExpr TypeExpr
  typeExpr_ = L.lens getter setter
    where
      getter :: TExpr -> TypeExpr
      getter (TConstant _ x) = x
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
      getter (TLetRecIn _ _ _ x) = x
      getter (TList _ x) = x
      getter (TArray _ x) = x
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
      setter (TLetRecIn e f g _) x = TLetRecIn e f g x
      setter (TList e _) x = TList e x
      setter (TArray e _) x = TArray e x

toExpr :: TExpr -> Expr
toExpr (TConstant x _) = Constant x
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
toExpr (TLetRecIn x y z _) = LetRecIn x (toExpr y) (toExpr z)
-- toExpr (TTypeDecl x y _) = TypeDecl x y
toExpr (TList e _) = List (fmap toExpr e)
toExpr (TArray e _) = Array (fmap toExpr e)

pattern IntC :: Int -> Expr
pattern IntC x = Constant (IntVal x)

pattern BoolC :: Bool -> Expr
pattern BoolC x = Constant (BoolVal x)

pattern V :: Text -> Expr
pattern V x = Var (Sym x)

pattern (:*) :: Expr -> Expr -> Expr
pattern l :* r = InfixOpExpr l Mul r

pattern (:+) :: Expr -> Expr -> Expr
pattern l :+ r = InfixOpExpr l Plus r

pattern (:-) :: Expr -> Expr -> Expr
pattern l :- r = InfixOpExpr l Minus r

pattern (:/) :: Expr -> Expr -> Expr
pattern l :/ r = InfixOpExpr l Div r

pattern (:*.) :: Expr -> Expr -> Expr
pattern l :*. r = InfixOpExpr l MulDot r

pattern (:+.) :: Expr -> Expr -> Expr
pattern l :+. r = InfixOpExpr l PlusDot r

pattern (:-.) :: Expr -> Expr -> Expr
pattern l :-. r = InfixOpExpr l MinusDot r

pattern (:/.) :: Expr -> Expr -> Expr
pattern l :/. r = InfixOpExpr l DivDot r

pattern (:<) :: Expr -> Expr -> Expr
pattern l :< r = InfixOpExpr l (Compare LessThan) r

pattern (:<=) :: Expr -> Expr -> Expr
pattern l :<= r = InfixOpExpr l (Compare LessThanEq) r

pattern (:==) :: Expr -> Expr -> Expr
pattern l :== r = InfixOpExpr l (Compare Equal) r

pattern (:>) :: Expr -> Expr -> Expr
pattern l :> r = InfixOpExpr l (Compare GreaterThan) r

pattern (:>=) :: Expr -> Expr -> Expr
pattern l :>= r = InfixOpExpr l (Compare GreaterThanEq) r

pattern (:&&) :: Expr -> Expr -> Expr
pattern l :&& r = InfixOpExpr l BoolAnd r

pattern (:||) :: Expr -> Expr -> Expr
pattern l :|| r = InfixOpExpr l BoolOr r

pattern (:%) :: Expr -> Expr -> Expr
pattern l :% r = InfixOpExpr l Mod r

pattern TIntC :: Int -> TExpr
pattern TIntC x = TConstant (IntVal x) (TypeAtom "int")

pattern TBoolC :: Bool -> TExpr
pattern TBoolC x = TConstant (BoolVal x) (TypeAtom "Bool")

pattern (:*:) :: TExpr -> TExpr -> TExpr
pattern l :*: r = TInfixOpExpr l Mul r (TypeAtom "int")

pattern (:+:) :: TExpr -> TExpr -> TExpr
pattern l :+: r = TInfixOpExpr l Plus r (TypeAtom "int")

pattern (:-:) :: TExpr -> TExpr -> TExpr
pattern l :-: r = TInfixOpExpr l Minus r (TypeAtom "int")

pattern (:/:) :: TExpr -> TExpr -> TExpr
pattern l :/: r = TInfixOpExpr l Div r (TypeAtom "int")

pattern (:*.:) :: TExpr -> TExpr -> TExpr
pattern l :*.: r = TInfixOpExpr l MulDot r (TypeAtom "float")

pattern (:+.:) :: TExpr -> TExpr -> TExpr
pattern l :+.: r = TInfixOpExpr l PlusDot r (TypeAtom "float")

pattern (:-.:) :: TExpr -> TExpr -> TExpr
pattern l :-.: r = TInfixOpExpr l MinusDot r (TypeAtom "float")

pattern (:/.:) :: TExpr -> TExpr -> TExpr
pattern l :/.: r = TInfixOpExpr l DivDot r (TypeAtom "float")

pattern (:<:) :: TExpr -> TExpr -> TExpr
pattern l :<: r = TInfixOpExpr l (Compare LessThan) r (TypeAtom "bool")

pattern (:<=:) :: TExpr -> TExpr -> TExpr
pattern l :<=: r = TInfixOpExpr l (Compare LessThanEq) r (TypeAtom "bool")

pattern (:==:) :: TExpr -> TExpr -> TExpr
pattern l :==: r = TInfixOpExpr l (Compare Equal) r (TypeAtom "bool")

pattern (:>:) :: TExpr -> TExpr -> TExpr
pattern l :>: r = TInfixOpExpr l (Compare GreaterThan) r (TypeAtom "bool")

pattern (:>=:) :: TExpr -> TExpr -> TExpr
pattern l :>=: r = TInfixOpExpr l (Compare GreaterThanEq) r (TypeAtom "bool")

pattern (:&&:) :: TExpr -> TExpr -> TExpr
pattern l :&&: r = TInfixOpExpr l BoolAnd r (TypeAtom "bool")

pattern (:||:) :: TExpr -> TExpr -> TExpr
pattern l :||: r = TInfixOpExpr l BoolOr r (TypeAtom "bool")

pattern (:%:) :: TExpr -> TExpr -> TExpr
pattern l :%: r = TInfixOpExpr l Mod r (TypeAtom "int")
