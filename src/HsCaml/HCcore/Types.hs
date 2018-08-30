{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module HsCaml.HCcore.Types where
import HsCaml.FrontEnd.Types hiding (Expr, TExpr)
import Control.Lens
import Data.Text

data CRValue = CRConst Value
             | CRVar Sym
             | CFunApply CLValue [CLValue]
             | CInfixOpExpr CLValue InfixOp CLValue
             deriving (Show, Eq)

data CLValue = CLConst Value TypeExpr
             | CLVar Sym TypeExpr
             deriving (Show, Eq)

fromLValue :: CLValue -> CRValue
fromLValue (CLConst v _) = CRConst v
fromLValue (CLVar v _) = CRVar v

newtype DCId = DCId Int deriving (Show, Eq, Ord)

newtype CType = CType Name deriving (Show, Eq, Ord)

data CDataCnstr = CDataCnstr Name [TypeExpr] DCId deriving (Show)

data CExpr = CMultiExpr [CExpr] TypeExpr
           | CMatch CLValue [(Pattern, CExpr)] TypeExpr
           | CLetRec LetPattern CExpr TypeExpr
           | CLetRecIn LetPattern CExpr CExpr TypeExpr
           | CInitialize CLValue CRValue TypeExpr
           | CValue CLValue TypeExpr
           | CWhile CExpr CExpr TypeExpr
           | CRuntimeError Text TypeExpr
           | CApply CLValue [CLValue] TypeExpr
           deriving (Show, Eq)

data CTypeDecl = CTypeDecl CType [CDataCnstr]
               deriving(Show)

data CTopLevel = CTopLevelExpr CExpr
               | CTopLevelTypeDecl
               deriving(Show)

instance HasTypeExpr CExpr where
  typeExpr_ = lens getter setter
    where
      getter (CMultiExpr _ t) = t
      getter (CMatch _ _ t) = t
      getter (CLetRec _ _ t) = t
      getter (CLetRecIn _ _ _ t) = t
      getter (CInitialize _ _ t) = t
      getter (CValue _ t) = t
      getter (CWhile _ _ t) = t
      getter (CRuntimeError _ t) = t
      getter (CApply _ _ t) = t
      setter (CMultiExpr a _) t = CMultiExpr a t
      setter (CMatch v xs _) t = CMatch v xs t
      setter (CLetRec a b _) t = CLetRec a b t
      setter (CLetRecIn a b c _) t = CLetRecIn a b c t
      setter (CInitialize a b _) t = CInitialize a b t
      setter (CValue a _) t = CValue a t
      setter (CWhile a b _) t = CWhile a b t
      setter (CRuntimeError a _) t = CRuntimeError a t
      setter (CApply a b _) t = CApply a b t

instance HasTypeExpr CLValue where
  typeExpr_ = lens getter setter
    where
      getter (CLConst _ t) = t
      getter (CLVar _ t) = t
      setter (CLConst x _) t = CLConst x t
      setter (CLVar x _) t = CLVar x t
-- getLastSym :: CExpr -> CLValue
