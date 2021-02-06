{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module HsCaml.FrontEnd.Types where

import Data.String
import Data.Text
import Deriving.Show.Simple
import GHC.Generics (Generic)
import TextShow

type Name = Text

newtype Sym = Sym
  { name :: Name
  }
  deriving (Eq, Ord, Generic, TextShow)
  deriving (Show) via (WrapSimple Sym)

instance IsString Sym where
  fromString = Sym . pack

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

-- 型付いた式
data Expr
  = Constant {val :: Value, typeExpr :: TypeExpr}
  | Var {sym :: Sym, typeExpr :: TypeExpr}
  | Paren {body :: Expr, typeExpr :: TypeExpr}
  | InfixOpExpr {lhs :: Expr, op :: InfixOp, rhs :: Expr, typeExpr :: TypeExpr}
  | BegEnd {body :: Expr, typeExpr :: TypeExpr}
  | MultiExpr {bodies :: [Expr], typeExpr :: TypeExpr}
  | Constr {arg :: Expr, typeExpr :: TypeExpr}
  | IfThenElse {if_ :: Expr, then_ :: Expr, else_ :: Expr, typeExpr :: TypeExpr}
  | Match {var :: Expr, pats :: [(Pattern, Expr)], typeExpr :: TypeExpr}
  | While {cond :: Expr, body :: Expr, typeExpr :: TypeExpr}
  | FunApply {fun :: Expr, args :: [Expr], typeExpr :: TypeExpr}
  | Let {pat :: LetPattern, body :: Expr, typeExpr :: TypeExpr}
  | LetRec {pat :: LetPattern, body :: Expr, typeExpr :: TypeExpr}
  | LetIn {pat :: LetPattern, rhs :: Expr, body :: Expr, typeExpr :: TypeExpr}
  | LetRecIn {pat :: LetPattern, rhs :: Expr, body :: Expr, typeExpr :: TypeExpr}
  | List {bodies :: [Expr], typeExpr :: TypeExpr}
  | Array {bodies :: [Expr], typeExpr :: TypeExpr}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (WrapSimple Expr)

data TopLevel
  = TopLevelExpr Expr
  | TopLevelTypeDecl TypeDecl
  deriving (Show, Eq)

newtype Statement = Statement [TopLevel] deriving (Show, Eq)

data TStatement f = TStatement [Expr] [TypeDecl]

data LetPattern
  = FuncLetPattern
      { lpatType :: TypeExpr,
        funcsym :: Sym,
        args :: [(Sym, TypeExpr)]
      }
  | LetPattern
      { lpatType :: TypeExpr,
        pat :: Pattern
      }
  deriving (Eq, Ord, Generic)
  deriving (Show) via (WrapSimple LetPattern)

-- lpattypeとmpattypeがdupでは？？？？
-- 手での型指定はUnspecifiedTypeでなくすればよさそう
data Pattern
  = ConstantPattern
      { mpatType :: TypeExpr,
        val :: Value
      }
  | ParenPattern
      { mpatType :: TypeExpr,
        mpat :: Pattern
      }
  | ListPattern
      { mpatType :: TypeExpr,
        patterns :: [Pattern]
      }
  | OrPattern
      { mpatType :: TypeExpr,
        left :: Pattern,
        right :: Pattern
      }
  | VarPattern
      { mpatType :: TypeExpr,
        sym :: Sym
      }
  | ConstrPattern
      { mpatType :: TypeExpr,
        constr :: Sym,
        rest :: Pattern,
        dcId :: Maybe Int
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

class HasPatType a where
  patType :: a -> TypeExpr

instance HasPatType LetPattern where
  patType = lpatType

instance HasPatType Pattern where
  patType = mpatType

pattern IntC :: Int -> Expr
pattern IntC x = Constant (IntVal x) UnspecifiedType

pattern BoolC :: Bool -> Expr
pattern BoolC x = Constant (BoolVal x) UnspecifiedType

pattern V :: Text -> Expr
pattern V x = Var (Sym x) UnspecifiedType

pattern (:*) :: Expr -> Expr -> Expr
pattern l :* r = InfixOpExpr l Mul r UnspecifiedType

pattern (:+) :: Expr -> Expr -> Expr
pattern l :+ r = InfixOpExpr l Plus r UnspecifiedType

pattern (:-) :: Expr -> Expr -> Expr
pattern l :- r = InfixOpExpr l Minus r UnspecifiedType

pattern (:/) :: Expr -> Expr -> Expr
pattern l :/ r = InfixOpExpr l Div r UnspecifiedType

pattern (:*.) :: Expr -> Expr -> Expr
pattern l :*. r = InfixOpExpr l MulDot r UnspecifiedType

pattern (:+.) :: Expr -> Expr -> Expr
pattern l :+. r = InfixOpExpr l PlusDot r UnspecifiedType

pattern (:-.) :: Expr -> Expr -> Expr
pattern l :-. r = InfixOpExpr l MinusDot r UnspecifiedType

pattern (:/.) :: Expr -> Expr -> Expr
pattern l :/. r = InfixOpExpr l DivDot r UnspecifiedType

pattern (:<) :: Expr -> Expr -> Expr
pattern l :< r = InfixOpExpr l (Compare LessThan) r UnspecifiedType

pattern (:<=) :: Expr -> Expr -> Expr
pattern l :<= r = InfixOpExpr l (Compare LessThanEq) r UnspecifiedType

pattern (:==) :: Expr -> Expr -> Expr
pattern l :== r = InfixOpExpr l (Compare Equal) r UnspecifiedType

pattern (:>) :: Expr -> Expr -> Expr
pattern l :> r = InfixOpExpr l (Compare GreaterThan) r UnspecifiedType

pattern (:>=) :: Expr -> Expr -> Expr
pattern l :>= r = InfixOpExpr l (Compare GreaterThanEq) r UnspecifiedType

pattern (:&&) :: Expr -> Expr -> Expr
pattern l :&& r = InfixOpExpr l BoolAnd r UnspecifiedType

pattern (:||) :: Expr -> Expr -> Expr
pattern l :|| r = InfixOpExpr l BoolOr r UnspecifiedType

pattern (:%) :: Expr -> Expr -> Expr
pattern l :% r = InfixOpExpr l Mod r UnspecifiedType

pattern TIntC :: Int -> Expr
pattern TIntC x = Constant (IntVal x) (TypeAtom "int")

pattern TBoolC :: Bool -> Expr
pattern TBoolC x = Constant (BoolVal x) (TypeAtom "bool")

tIntVar :: Text -> Expr
tIntVar s = Var (Sym s) (TypeAtom "int")

pattern (:*:) :: Expr -> Expr -> Expr
pattern l :*: r = InfixOpExpr l Mul r (TypeAtom "int")

pattern (:+:) :: Expr -> Expr -> Expr
pattern l :+: r = InfixOpExpr l Plus r (TypeAtom "int")

pattern (:-:) :: Expr -> Expr -> Expr
pattern l :-: r = InfixOpExpr l Minus r (TypeAtom "int")

pattern (:/:) :: Expr -> Expr -> Expr
pattern l :/: r = InfixOpExpr l Div r (TypeAtom "int")

pattern (:*.:) :: Expr -> Expr -> Expr
pattern l :*.: r = InfixOpExpr l MulDot r (TypeAtom "float")

pattern (:+.:) :: Expr -> Expr -> Expr
pattern l :+.: r = InfixOpExpr l PlusDot r (TypeAtom "float")

pattern (:-.:) :: Expr -> Expr -> Expr
pattern l :-.: r = InfixOpExpr l MinusDot r (TypeAtom "float")

pattern (:/.:) :: Expr -> Expr -> Expr
pattern l :/.: r = InfixOpExpr l DivDot r (TypeAtom "float")

pattern (:<:) :: Expr -> Expr -> Expr
pattern l :<: r = InfixOpExpr l (Compare LessThan) r (TypeAtom "bool")

pattern (:<=:) :: Expr -> Expr -> Expr
pattern l :<=: r = InfixOpExpr l (Compare LessThanEq) r (TypeAtom "bool")

pattern (:==:) :: Expr -> Expr -> Expr
pattern l :==: r = InfixOpExpr l (Compare Equal) r (TypeAtom "bool")

pattern (:>:) :: Expr -> Expr -> Expr
pattern l :>: r = InfixOpExpr l (Compare GreaterThan) r (TypeAtom "bool")

pattern (:>=:) :: Expr -> Expr -> Expr
pattern l :>=: r = InfixOpExpr l (Compare GreaterThanEq) r (TypeAtom "bool")

pattern (:&&:) :: Expr -> Expr -> Expr
pattern l :&&: r = InfixOpExpr l BoolAnd r (TypeAtom "bool")

pattern (:||:) :: Expr -> Expr -> Expr
pattern l :||: r = InfixOpExpr l BoolOr r (TypeAtom "bool")

pattern (:%:) :: Expr -> Expr -> Expr
pattern l :%: r = InfixOpExpr l Mod r (TypeAtom "int")
