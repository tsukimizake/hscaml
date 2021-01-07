module HsCaml.TypeChecker.UFUtil (TypecheckM (..), union, find, enterLevel, leaveLevel, currentLevel) where

import Control.Monad.State
import qualified Data.Extensible as E
import qualified Data.Extensible.Effect as E
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Data.Proxy
import qualified HsCaml.Common.Gensym as GS
import HsCaml.FrontEnd.Types

type UF = M.Map TypeExpr TypeExpr

data TypecheckState = TypecheckState
  { uf_ :: UF,
    level_ :: Level
  }

type TypecheckEff = E.Eff '["gs" E.>: GS.GensymM, "err" E.>: E.EitherEff CompileError, "uf" E.>: TypecheckM]

newtype TypecheckM a = TypecheckM
  { runTypecheckM :: State TypecheckState a
  }
  deriving (Functor, Applicative, Monad, MonadState TypecheckState)

-- UnionFind

getUF :: TypecheckEff UF
getUF = do
  E.liftEff (Proxy @"uf") $ gets uf_

setUF :: UF -> TypecheckEff ()
setUF uf = do
  TypecheckState _ l <- E.liftEff (Proxy @"uf") get
  E.liftEff (Proxy @"uf") $ put $ TypecheckState uf l

setParent :: TypeExpr -> TypeExpr -> TypecheckEff ()
setParent v p = do
  uf <- getUF
  setUF $ M.insert v p uf

find :: TypeExpr -> TypecheckEff TypeExpr
find tv = do
  uf <- getUF
  let p = Maybe.fromMaybe tv (uf M.!? tv)
  parent <-
    if p /= tv
      then find p
      else pure p
  setParent tv parent
  pure parent

union :: TypeExpr -> TypeExpr -> TypecheckEff ()
union l r = do
  uf <- getUF
  lp <- find l
  rp <- find r
  case (lp, rp) of
    (lv@(TypeVar _ _), rv@(TypeVar _ _)) ->
      setUF $ M.insert lv rv uf
    (lt, rv@(TypeVar _ _)) ->
      setUF $ M.insert rv lt uf
    (lv@(TypeVar _ _), rt) ->
      setUF $ M.insert lv rt uf
    (_, _) ->
      error "それっておかしくねえ？"

-- LEVEL

enterLevel :: TypecheckEff ()
enterLevel = do
  TypecheckState u l <- E.liftEff (Proxy @"uf") get
  E.liftEff (Proxy @"uf") $ put $ TypecheckState u (l + 1)

leaveLevel :: TypecheckEff ()
leaveLevel = do
  TypecheckState u l <- E.liftEff (Proxy @"uf") get
  E.liftEff (Proxy @"uf") $ put $ TypecheckState u (l - 1)

currentLevel :: TypecheckEff Level
currentLevel = do
  E.liftEff (Proxy @"uf") $ gets level_
