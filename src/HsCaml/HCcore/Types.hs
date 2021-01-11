{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module HsCaml.HCcore.Types where

import Data.Text
import HsCaml.FrontEnd.Types hiding (Expr, TExpr)

data CRValue
  = CRConst Value
  | CRVar Sym
  | CFunApply CLValue [CLValue]
  | CInfixOpExpr CLValue InfixOp CLValue
  deriving (Show, Eq)

data CLValue
  = CLConst Value TypeExpr
  | CLVar Sym TypeExpr
  deriving (Show, Eq)

fromLValue :: CLValue -> CRValue
fromLValue (CLConst v _) = CRConst v
fromLValue (CLVar v _) = CRVar v

newtype DCId = DCId Int deriving (Show, Eq, Ord)

newtype CType = CType Name deriving (Show, Eq, Ord)

data CDataCnstr = CDataCnstr Name [TypeExpr] DCId deriving (Show)

data CExpr
  = CMultiExpr {bodies :: [CExpr], typeExpr :: TypeExpr}
  | CMatch {val :: CLValue, pats :: [(Pattern, CExpr)], typeExpr :: TypeExpr}
  | CLetRec {pat :: LetPattern, body :: CExpr, typeExpr :: TypeExpr}
  | CLetRecIn {pat :: LetPattern, header :: CExpr, body :: CExpr, typeExpr :: TypeExpr}
  | CInitialize {lhs :: CLValue, rhs :: CRValue, typeExpr :: TypeExpr}
  | CValue {val :: CLValue, typeExpr :: TypeExpr}
  | CWhile {cond :: CExpr, body :: CExpr, typeExpr :: TypeExpr}
  | CRuntimeError {err :: Text, typeExpr :: TypeExpr}
  | CApply {fun :: CLValue, args :: [CLValue], typeExpr :: TypeExpr}
  | CList {values :: [CLValue], typeExpr :: TypeExpr}
  | CArray {arr :: [CLValue], typeExpr :: TypeExpr}
  deriving (Show, Eq)

data CTypeDecl = CTypeDecl CType [CDataCnstr]
  deriving (Show)

data CTopLevel
  = CTopLevelExpr CExpr
  | CTopLevelTypeDecl
  deriving (Show)
