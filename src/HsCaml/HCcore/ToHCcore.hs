{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- HCcoreでは数式や関数適用をバラしたりコンストラクタにタグつけたりする
module HsCaml.HCcore.ToHCcore where

import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Reader
import Data.Extensible
import Data.Extensible.Effect
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import qualified HsCaml.Common.Gensym as GS
import HsCaml.FrontEnd.OCamlType
import HsCaml.FrontEnd.Types as Types
import HsCaml.HCcore.Types
import TextShow

type ToHCcoreM = Eff '["gs" >: GS.GensymM, "err" >: EitherEff CompileError, "te" >: ReaderEff TypeEnv]

runToHCcoreM :: TypeEnv -> ToHCcoreM a -> Either CompileError (a, GS.GensymState)
runToHCcoreM te =
  leaveEff
    . flip runReaderEff te
    . runEitherEff
    . flip runStateEff GS.initialGensymState

-- newtype ToHCcoreM a = ToHCcoreM {runToHCcoreMImpl :: GS.GensymT (Either CompileError) a}
--                     deriving (Functor, Applicative, Monad)

-- runToHCcoreM :: a -> (a -> ToHCcoreM b) -> Either CompileError b
-- runToHCcoreM e impl = GS.runGensymT . flip runStateDef GS.initialGensymState $ (impl e)

toFlatMultiExpr :: [CExpr] -> CExpr
toFlatMultiExpr xs = CMultiExpr (impl xs) (typeOfLastExpr xs)
  where
    typeOfLastExpr :: [CExpr] -> TypeExpr
    typeOfLastExpr [] = error "typeOfLastExpr of empty list"
    typeOfLastExpr [x] = x.typeExpr
    typeOfLastExpr (_ : xs) = typeOfLastExpr xs
    impl :: [CExpr] -> [CExpr]
    impl [] = []
    impl ((CMultiExpr xs _) : rest) = xs ++ impl rest
    impl (x : xs) = x : impl xs

toHCcore :: Expr -> TypeEnv -> Either CompileError CExpr
toHCcore e te = do
  (xs, _) <- runToHCcoreM te (toHCcoreM Nothing e)
  pure $ toFlatMultiExpr [xs]

makeCTypeDecl :: TypeDecl -> CTypeDecl
makeCTypeDecl (TypeDecl name xs) = CTypeDecl (CType name) (setDCId 0 xs)
  where
    setDCId :: Int -> [DataCnstr] -> [CDataCnstr]
    setDCId _ [] = []
    setDCId n ((DataCnstr dcname args) : rest) = (CDataCnstr dcname args (DCId n)) : setDCId (n + 1) rest

genSym :: T.Text -> ToHCcoreM Sym
genSym s = do
  liftEff (Proxy :: Proxy "gs") $ GS.genSym s

genSymLVar :: TypeExpr -> ToHCcoreM CLValue
genSymLVar t = do
  sym <- liftEff (Proxy :: Proxy "gs") $ GS.genSym ""
  pure $ wrap sym
  where
    wrap x = CLVar x t

-- getLastSym :: CExprWithRet -> CLValue
-- getLastSym (CMultiExpr [x]) = x ^. ret_
-- getLastSym (CMultiExpr xs) = getLastSym $ CMultiExpr $ tail xs
-- getLastSym ce = undefined

texprsTocexprs :: Maybe CLValue -> [Expr] -> ToHCcoreM [CExpr]

texprsTOcexprs _ [] = pure $ CMultiExpr []

texprsTocexprs ret [x] = do
  res <- toHCcoreM ret x
  pure [res]
texprsTocexprs ret (x : xs) = do
  hd <- toHCcoreM Nothing x
  rest <- texprsTocexprs ret xs
  pure $ hd : rest

-- 式の返り値がある場合CLValueを返す。CExprはputCExprで構築する
-- 第一引数は返り値を入れるシンボル
toHCcoreM :: Maybe CLValue -> Expr -> ToHCcoreM CExpr
toHCcoreM ret e@(While a b t) = do
  aret <- genSymLVar ocamlBool
  a' <- toHCcoreM (Just aret) a
  b' <- toHCcoreM ret b
  pure $ CWhile a' b' t
-- aが関数適用ならバラす
-- (f x) y を
-- g <- f x
-- g y
-- のように 値呼びなので引数も同様に
toHCcoreM ret e@(FunApply f args t) = do
  fret <- genSymLVar $ f.typeExpr
  f' <- toHCcoreM (Just fret) f
  argPrimeWithArgRets <- forM args $ \arg -> do
    argret <- genSymLVar $ arg.typeExpr
    arg' <- toHCcoreM (Just argret) arg
    pure (arg', argret)
  let args' = fmap fst argPrimeWithArgRets :: [CExpr]
  let argrets = fmap snd argPrimeWithArgRets :: [CLValue]
  pure $ CMultiExpr (f' : args' <> [CApply fret argrets t]) t
toHCcoreM ret e@(List xs t) = do
  ys <- forM (init xs) $ \x -> do
    xret <- genSymLVar (x.typeExpr)
    x' <- toHCcoreM (Just xret) x
    pure (x', xret)
  last <- toHCcoreM ret $ last xs
  pure $ CMultiExpr (fmap fst ys <> [CList (fmap snd ys) t] <> [last]) t
toHCcoreM ret e@(Array xs t) = do
  ys <- forM (init xs) $ \x -> do
    xret <- genSymLVar (x.typeExpr)
    x' <- toHCcoreM (Just xret) x
    pure (x', xret)
  last <- toHCcoreM ret $ last xs
  pure $ CMultiExpr (fmap fst ys <> [CArray (fmap snd ys) t] <> [last]) t
toHCcoreM ret e@(Let s b t) = do
  b' <- toHCcoreM ret b
  pure $ CLetRec s b' t
toHCcoreM ret e@(LetRec s b t) = do
  bret <- genSymLVar (b.typeExpr)
  b' <- toHCcoreM (Just bret) b
  pure $ CLetRec s b' t
toHCcoreM ret e@(LetIn s b r t) = toHCcoreM ret $ LetRecIn s b r t
toHCcoreM ret e@(LetRecIn s b r t) = do
  bret <- genSymLVar (b.typeExpr)
  b' <- toHCcoreM (Just bret) b
  r' <- toHCcoreM ret r
  pure $ CLetRecIn s b' r' t
toHCcoreM ret (Constant v t) = pure $ CValue (CLConst v t) t
toHCcoreM ret e@(Var v t) = pure $ CValue (CLVar v t) t
toHCcoreM ret (Paren e _) = toHCcoreM ret e
toHCcoreM ret (InfixOpExpr l op r t) = do
  currentname <- genSym ""
  lret <- genSymLVar $ l.typeExpr
  l' <- toHCcoreM (Just lret) l
  rret <- genSymLVar $ r.typeExpr
  r' <- toHCcoreM (Just rret) r

  ret' <- case ret of
    Nothing -> genSymLVar t
    Just x -> pure x
  let res =
        CMultiExpr
          [ l',
            r',
            CInitialize
              ret'
              (CInfixOpExpr lret op rret)
              t
          ]
          t
  pure res
toHCcoreM ret (BegEnd x _) = toHCcoreM ret x
toHCcoreM ret (MultiExpr xs t) = do
  inites <- forM (init xs) $ \x ->
    toHCcoreM Nothing x
  laste <- toHCcoreM ret (last xs)
  pure $ CMultiExpr (inites <> [laste]) t
toHCcoreM ret (IfThenElse a b c t) = do
  aret <- genSymLVar (a.typeExpr)
  a' <- toHCcoreM (Just aret) a
  bret <- genSymLVar (b.typeExpr)
  b' <- toHCcoreM (Just bret) b
  cret <- genSymLVar (c.typeExpr)
  c' <- toHCcoreM (Just cret) c
  ret <- genSymLVar (c.typeExpr)
  let matchExpr =
        CMatch
          aret
          [ (ConstantPattern ocamlBool (BoolVal True), CValue bret (b.typeExpr)),
            (ConstantPattern ocamlBool (BoolVal False), CValue cret (c.typeExpr))
          ]
          t

  let res =
        CMultiExpr
          [ a',
            matchExpr,
            CInitialize ret (fromLValue bret) (b.typeExpr),
            CInitialize ret (fromLValue cret) (c.typeExpr)
          ]
          t
  pure res
toHCcoreM ret (Constr _ _) = throwSemanticsError "data constructor is not yet implemented!"
toHCcoreM ret (Types.Match e pats t) = do
  eret <- genSymLVar (e.typeExpr)
  e' <- toHCcoreM (Just eret) e
  retSym <- case ret of
    Just x -> pure x
    Nothing -> genSymLVar t
  pats' <- forM pats $ \pat ->
    matchCaseToHCcoreM eret retSym pat t
  pure $ CMatch eret pats' t

throwSemanticsError :: T.Text -> ToHCcoreM a
throwSemanticsError s = throwEff (Proxy :: Proxy "err") $ SemanticsError s

matchCaseToHCcoreM :: CLValue -> CLValue -> (Pattern, Expr) -> TypeExpr -> ToHCcoreM (Pattern, CExpr)
matchCaseToHCcoreM x ret (pat, body) t = do
  body' <- toHCcoreM (Just ret) body
  case pat of
    ConstantPattern _ _ ->
      pure
        ( pat,
          CMultiExpr
            [ body',
              CInitialize ret (fromLValue ret) t
            ]
            t
        )
    VarPattern pt v ->
      pure
        ( pat,
          CMultiExpr
            [ CInitialize (CLVar v pt) (fromLValue x) pt,
              body',
              CInitialize ret (fromLValue ret) t
            ]
            t
        )
    ParenPattern _ innerpat ->
      matchCaseToHCcoreM x ret (innerpat, body) t
    ListPattern pt pats ->
      pure (pat, body')
    OrPattern pt l r ->
      pure (pat, body')
    ConstrPattern pt cnstrSym rest _ -> do
      typeEnv <- askEff (Proxy :: Proxy "te")
      dcId <- getDataCnstrId typeEnv cnstrSym
      pure (pat{dcId = Just dcId}, body')

getDataCnstrId :: TypeEnv -> Sym -> ToHCcoreM Int
getDataCnstrId (TypeEnv tds) name = do
  let resl = mapMaybe (getDataCnstrIdImpl 0 name) tds
  case length resl of
    0 -> throwSemanticsError $ " data constructor " <> name <> " not found in type " <> name
    1 -> pure . fromJust . listToMaybe $ resl
    n -> throwSemanticsError $ " data constructor " <> name <> " found " <> showt n <> " times in type " <> name
  where
    getDataCnstrIdImpl :: Int -> Sym -> TypeDecl -> Maybe Int
    getDataCnstrIdImpl n nameToSearch (TypeDecl name []) = Nothing
    getDataCnstrIdImpl n nameToSearch (TypeDecl name (x : xs)) =
      if name == nameToSearch
        then pure n
        else getDataCnstrIdImpl (n + 1) nameToSearch (TypeDecl name xs)
