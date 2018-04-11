{-# OPTIONS -Wall #-}
module HsCaml.HCcore.Types where
import HsCaml.FrontEnd.Types hiding (Expr, TExpr)

data CAtom = CConstant Value TypeExpr
           | CVar Sym TypeExpr
           deriving (Show)

newtype DCId = DCId Int deriving (Show, Eq, Ord)
newtype CType = CType Name deriving (Show, Eq, Ord)

data CDataCnstr = CDataCnstr Name [TypeExpr] DCId deriving (Show)

data CExpr = CInfixOpExpr CAtom InfixOp CAtom
           | CAtom CAtom
           | CMultiExpr [CExpr] TypeExpr
           | CIfThenElse CAtom CAtom CAtom TypeExpr
           | CFunApply CAtom [CAtom] TypeExpr
           | CTypeDecl CType [CDataCnstr]
           | CLetRec CAtom CExpr TypeExpr
           | CLetRecIn CAtom CExpr CAtom TypeExpr
           deriving (Show)
