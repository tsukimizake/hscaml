module HsCaml.TypeChecker.UFUtil (TypecheckM (..), union, find, enterLevel, leaveLevel, currentLevel) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import HsCaml.FrontEnd.Types

type UF = M.Map TypeExpr TypeExpr

data TypecheckState = TypecheckState
  { uf_ :: UF,
    level_ :: Level
  }

newtype TypecheckM a = TypecheckM
  { runTypecheckM :: State TypecheckState a
  }
  deriving (Functor, Applicative, Monad, MonadState TypecheckState)

-- UnionFind

getUF :: TypecheckM UF
getUF = do
  gets uf_

setUF :: UF -> TypecheckM ()
setUF uf = do
  TypecheckState _ l <- get
  put $ TypecheckState uf l

setParent :: TypeExpr -> TypeExpr -> TypecheckM ()
setParent v p = do
  uf <- getUF
  setUF $ M.insert v p uf

find :: TypeExpr -> TypecheckM TypeExpr
find tv = do
  uf <- getUF
  let p = Maybe.fromMaybe tv (uf M.!? tv)
  parent <-
    if p /= tv
      then find p
      else pure p
  setParent tv parent
  pure parent

union :: TypeExpr -> TypeExpr -> TypecheckM ()
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

enterLevel :: TypecheckM ()
enterLevel = do
  TypecheckState u l <- get
  put $ TypecheckState u (l + 1)

leaveLevel :: TypecheckM ()
leaveLevel = do
  TypecheckState u l <- get
  put $ TypecheckState u (l - 1)

currentLevel :: TypecheckM Level
currentLevel = do
  gets level_
