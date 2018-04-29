{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HsCaml.HCcore.ToHCcore where
import HsCaml.FrontEnd.Types
import HsCaml.HCcore.Types
import qualified HsCaml.Common.Gensym as GS
import Data.Text
import Control.Monad.State.Strict
import Control.Lens.Operators
import Data.Maybe
import Data.Monoid
import qualified Control.Lens as L

data CExprWithRet = CExprWithRet
  {
    __cexpr :: CExpr,
    __ret :: CLValue
  }deriving(Show)
--
L.makeLenses ''CExprWithRet

newtype ToHCcoreM a = ToHCcoreM {runToHCcoreMImpl :: StateT [CExpr] GS.GensymM a}
                    deriving (Functor, Applicative, Monad, MonadState [CExpr])

toFlatMultiExpr :: [CExpr] -> CExpr
toFlatMultiExpr xs = CMultiExpr (impl xs) (typeOfLastExpr xs)
  where
    typeOfLastExpr :: [CExpr] -> TypeExpr
    typeOfLastExpr [] = error "typeOfLastExpr of empty list"
    typeOfLastExpr [x] = x ^. _typeExpr
    typeOfLastExpr (_:xs) = typeOfLastExpr xs
    impl :: [CExpr] -> [CExpr]
    impl [] = []
    impl ((CMultiExpr xs _):rest) = xs ++ impl rest
    impl (x:xs) = x : impl xs

-- getReturnedLVar :: CExpr -> ToHCcoreM CLValue
-- getReturnedLVar (CMultiExpr (head:_) _) = getReturnedLVar head
-- getReturnedLVar (CMultiExpr [] _) = error "empty multiexpr"
-- getReturnedLVar (CIfThenElse _ ethen eelse _) = do
--   lthen <- getReturnedLVar ethen
--   rthen <- getReturnedLVar eelse
--   if lthen == rthen
--     then pure $ lthen
--     else error "last sym of then and else are not equal"
-- getReturnedLVar (CLetRec v _ _) = pure v
-- getReturnedLVar (CLetRecIn _ _ v _) = pure v
-- getReturnedLVar (CInitialize assign _) = pure $ assign ^. _lhs
-- getReturnedLVar (CValue (CLConst c) t) = pure $ CLConst c t
-- getReturnedLVar (CValue (CLVar c) t) = pure $ CLVar c t
-- getReturnedLVar (CWhile _ body _) = getReturnedLVar body
-- getReturnedLVar (CValue (CFunApply _ _) _) = undefined
-- getReturnedLVar (CValue (CInfixOpExpr _ _ _) _) = undefined

putCExpr :: CExpr -> ToHCcoreM ()
putCExpr e = modify' (e:)

runToHCcoreM :: a -> (a -> ToHCcoreM b) -> b
runToHCcoreM e impl = let gxs = (evalStateT . runToHCcoreMImpl) (impl e) []
                      in GS.runGensymM gxs

toHCcore :: TExpr -> CExpr
toHCcore e = let gxs = (execStateT . runToHCcoreMImpl) (toHCcoreM e) [] :: GS.GensymM [CExpr]
                 xs = Prelude.reverse $ GS.runGensymM gxs :: [CExpr] -- TODO: MultiExpr の reverse
             in toFlatMultiExpr xs

-- (TypeDecl "expr"
--              [DataCnstr "Plus" [(TypeAtom "expr"), (TypeAtom "expr")],
--               DataCnstr "Minus" [(TypeAtom "expr"), (TypeAtom "expr")],
--               DataCnstr "Times" [(TypeAtom "expr"), (TypeAtom "expr")],
--               DataCnstr "Divide" [(TypeAtom "expr"), (TypeAtom "expr")],
--               DataCnstr "Value" [TypeAtom "string"]])
-- (CTypeDecl "expr"
--    [CDataCnstr "Plus" [TypeAtom "expr", TypeAtom "expr"] (DCId 0),
--     CDataCnstr "Plus" [TypeAtom "expr", TypeAtom "expr"] (DCId 1),
--     CDataCnstr "Plus" [TypeAtom "expr", TypeAtom "expr"] (DCId 2),
--     CDataCnstr "Plus" [TypeAtom "expr", TypeAtom "expr"] (DCId 3)
--     CDataCnstr "Value" [TypeAtom "string"] (DCId 4)])

makeCTypeDecl :: TypeDecl -> CTypeDecl
makeCTypeDecl (TypeDecl name xs) = CTypeDecl (CType name) (setDCId 0 xs)
  where
    setDCId :: Int -> [DataCnstr] -> [CDataCnstr]
    setDCId _ [] = []
    setDCId n ((DataCnstr dcname args):rest) = (CDataCnstr dcname args (DCId n)) : setDCId (n+1) rest

genSym :: Text -> ToHCcoreM Sym
genSym s = fmap Sym <$> ToHCcoreM $ lift $ GS.genSym s

genSymLVar :: TypeExpr -> ToHCcoreM CLValue
genSymLVar t = fmap wrap <$> ToHCcoreM $ lift $ GS.genSym ""
  where
    wrap x = CLVar (Sym x) t



-- reportNullPo :: Maybe CLValue -> TExpr -> ToHCcoreM ()
-- reportNullPo Nothing exp = error $ "return value of " <> show exp <> "is Nothing"
-- reprotNullPo _ _ = pure ()

-- destructComplexTExpr :: TExpr -> ToHCcoreM CExprWithRet
-- destructComplexTExpr (TConstant v t) = do
--   let r = (CLConst v) t
--   let res = CValue r t
--   putCExpr $ res
--   pure $ CExprWithRet res (Just r)
-- destructComplexTExpr (TVar s t) = do
--   let r = (CLVar s) t
--   let res = CValue r t
--   putCExpr $ res
--   pure $ CExprWithRet res (Just r)
-- destructComplexTExpr (TParen e _) = destructComplexTExpr e
-- destructComplexTExpr (TInfixOpExpr l op r t) = do
--   currentname <- genSym ""
--   l' <- destructComplexTExpr l
--   reportNullPo (l' ^. _ret) l
--   r' <- destructComplexTExpr r
--   reportNullPo (r' ^. _ret) r
--   pure $ CExprWithRet
--     (CMultiExpr
--       [
--         l' ^. _cexpr,
--         r' ^. _cexpr,
--         CInitialize (CAssign (CLVar currentname t) (CInfixOpExpr (fromJust $ l' ^. _ret) op (fromJust $ r' ^. _ret))) t
--       ] t)
--     (Just $ CLVar currentname t)
-- destructComplexTExpr (TBegEnd xs t) = destructComplexTExpr (TMultiExpr xs t)
-- destructComplexTExpr (TMultiExpr xs t) = do
--   ys <- mapM destructComplexTExpr xs
--   CMultiExpr ys t



-- 式の返り値がある場合CLValueを返す。CExprはputCExprで構築する
toHCcoreM :: TExpr -> ToHCcoreM CExprWithRet
toHCcoreM e@(TConstant _ _) = do
  res <- toHCcoreM e
  pure $ res
toHCcoreM e@(TVar _ _) = do
  res <- toHCcoreM e
  pure $ res
toHCcoreM (TParen e _) = toHCcoreM e
toHCcoreM (TInfixOpExpr l op r t) = do
  currentname <- genSym ""
  l' <- toHCcoreM l
  r' <- toHCcoreM r
  let res = CMultiExpr
        [
          l' ^. _cexpr,
          r' ^. _cexpr,
          CInitialize (CAssign
                       (CLVar currentname t)
                       (CInfixOpExpr (l' ^. _ret) op (r' ^. _ret))) t
        ] t
  pure $ CExprWithRet res (CLVar currentname t)
toHCcoreM (TBegEnd x _) = toHCcoreM x
toHCcoreM (TMultiExpr xs _) = do
  syms <- mapM toHCcoreM xs
  pure $ Prelude.last syms
toHCcoreM (TIfThenElse a b c t) = do
  a' <- toHCcoreM a
  b' <- toHCcoreM b
  c' <- toHCcoreM c
  ret <- genSymLVar (b ^. _typeExpr)
  let iteExpr =  CIfThenElse
        (a' ^. _ret)
        (CValue (b' ^. _ret) (b ^. _typeExpr))
        (CValue (c' ^. _ret) (c ^. _typeExpr)) t
  let res =
        CMultiExpr [
        a' ^. _cexpr,
        iteExpr,
        CInitialize (CAssign ret (fromLValue $ b' ^. _ret)) (b ^. _typeExpr),
        CInitialize (CAssign ret (fromLValue $ c' ^. _ret)) (c ^. _typeExpr)] t
  pure $ CExprWithRet res ret
toHCcoreM (TConstr _ _) = error "data constructor is not yet implemented!"
toHCcoreM (TMatch e pats t) = tMatchToHCcoreM e pats t
-- toHCcoreM ()

tMatchToHCcoreM :: TExpr -> [(Pattern, TExpr)] -> TypeExpr -> ToHCcoreM CExprWithRet
tMatchToHCcoreM  = undefined


-- ((((IntC 82) :* (IntC 3)) :+ (IntC 3)) :- (Paren ((IntC 2) :- (IntC 2))) :+ (Paren (IntC 2)))
-- a = ((IntC 82) :* (IntC 3))
-- b = (a :+ (IntC 3))
-- c = (Paren ((IntC 2) :- (IntC 2)))
-- d = b :- c
-- res = d :+ (IntC 2)
-- Atomでなかったらgensymしつつ下から構成していけばよさそう？ Parenの入れ子の扱いなどは大丈夫かチェックする必要がありそう
