{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module HsCaml.HCcore.ToHCcore where
import HsCaml.FrontEnd.Types
import HsCaml.HCcore.Types
import qualified HsCaml.Common.Gensym as GS
import Data.Extensible
import qualified Control.Lens as L
import Data.Text
import Control.Lens.Operators
import Data.Proxy
import Control.Monad
import HsCaml.FrontEnd.OCamlType

data CExprWithRet = CExprWithRet
  {
    _cexpr_ :: CExpr,
    _ret_ :: CLValue
  }deriving(Show)

L.makeLenses ''CExprWithRet

type ToHCcoreM = Eff '["gs" >: GS.GensymM, "err" >: EitherEff CompileError]

runToHCcoreM :: ToHCcoreM a -> Either CompileError (a, GS.GensymState)
runToHCcoreM = leaveEff
  . runEitherEff
  . flip runStateEff GS.initialGensymState

-- newtype ToHCcoreM a = ToHCcoreM {runToHCcoreMImpl :: GS.GensymMT (Either CompileError) a}
--                     deriving (Functor, Applicative, Monad)

-- runToHCcoreM :: a -> (a -> ToHCcoreM b) -> Either CompileError b
-- runToHCcoreM e impl = GS.runGensymMT . flip runStateDef GS.initialGensymState $ (impl e)

toFlatMultiExpr :: [CExpr] -> CExpr
toFlatMultiExpr xs = CMultiExpr (impl xs) (typeOfLastExpr xs)
  where
    typeOfLastExpr :: [CExpr] -> TypeExpr
    typeOfLastExpr [] = error "typeOfLastExpr of empty list"
    typeOfLastExpr [x] = x ^. typeExpr_
    typeOfLastExpr (_:xs) = typeOfLastExpr xs
    impl :: [CExpr] -> [CExpr]
    impl [] = []
    impl ((CMultiExpr xs _):rest) = xs ++ impl rest
    impl (x:xs) = x : impl xs

toHCcore :: TExpr -> Either CompileError CExpr
toHCcore e = do
  (xs, _) <- runToHCcoreM (toHCcoreM e)
  pure $ toFlatMultiExpr [xs ^. cexpr_]

makeCTypeDecl :: TypeDecl -> CTypeDecl
makeCTypeDecl (TypeDecl name xs) = CTypeDecl (CType name) (setDCId 0 xs)
  where
    setDCId :: Int -> [DataCnstr] -> [CDataCnstr]
    setDCId _ [] = []
    setDCId n ((DataCnstr dcname args):rest) = (CDataCnstr dcname args (DCId n)) : setDCId (n+1) rest

genSym :: Text -> ToHCcoreM Sym
genSym s = do
  sym <- liftEff (Proxy :: Proxy "gs") $ GS.genSym s
  pure $ Sym sym

genSymLVar :: TypeExpr -> ToHCcoreM CLValue
genSymLVar t = do
  sym <- liftEff (Proxy :: Proxy "gs") $ GS.genSym ""
  pure $ wrap sym
  where
    wrap x = CLVar (Sym x) t

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
          l' ^. cexpr_,
          r' ^. cexpr_,
          CInitialize (CAssign
                       (CLVar currentname t)
                       (CInfixOpExpr (l' ^. ret_) op (r' ^. ret_))) t
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
  ret <- genSymLVar (b ^. typeExpr_)
  -- let iteExpr =  CIfThenElse
  --       (a' ^. ret_)
  --       (CValue (b' ^. ret_) (b ^. typeExpr_))
  --       (CValue (c' ^. ret_) (c ^. typeExpr_)) t
  let matchExpr = CMatch (a' ^. ret_) [
        (ConstantPattern ocamlBool (BoolVal True),  (CValue (b' ^. ret_) (b ^. typeExpr_))),
        (ConstantPattern ocamlBool (BoolVal False), (CValue (c' ^. ret_) (c ^. typeExpr_)))
        ] t

  let res =
        CMultiExpr [
        a' ^. cexpr_,
        matchExpr,
        CInitialize (CAssign ret (fromLValue $ b' ^. ret_)) (b ^. typeExpr_),
        CInitialize (CAssign ret (fromLValue $ c' ^. ret_)) (c ^. typeExpr_)] t
  pure $ CExprWithRet res ret

toHCcoreM (TConstr _ _) = throwSemanticsError "data constructor is not yet implemented!"
toHCcoreM (TMatch e pats t) = do
  e' <- toHCcoreM e
  retSym <- genSymLVar t
  pats' <- forM pats $ \pat -> do
    matchCaseToHCcoreM (e' ^. ret_) retSym pat t
  pure $ CExprWithRet (CMatch (e' ^. ret_) pats' t) retSym

throwSemanticsError :: Text -> ToHCcoreM a
throwSemanticsError s = throwEff (Proxy :: Proxy "err") $ SemanticsError s

matchCaseToHCcoreM :: CLValue -> CLValue -> (Pattern, TExpr) -> TypeExpr -> ToHCcoreM (Pattern, CExpr)
matchCaseToHCcoreM x ret (pat, impl) t = do
  case pat of
    ConstantPattern pt v -> do
      cond <- genSymLVar ocamlBool
      impl' <- toHCcoreM impl
      pure $ (pat, CMultiExpr [(impl' ^. cexpr_), CInitialize (CAssign ret (fromLValue $ impl' ^. ret_)) pt] pt)


-- ((((IntC 82) :* (IntC 3)) :+ (IntC 3)) :- (Paren ((IntC 2) :- (IntC 2))) :+ (Paren (IntC 2)))
-- a = ((IntC 82) :* (IntC 3))
-- b = (a :+ (IntC 3))
-- c = (Paren ((IntC 2) :- (IntC 2)))
-- d = b :- c
-- res = d :+ (IntC 2)
-- Atomでなかったらgensymしつつ下から構成していけばよさそう？ Parenの入れ子の扱いなどは大丈夫かチェックする必要がありそう
