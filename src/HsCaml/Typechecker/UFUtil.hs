module HsCaml.TypeChecker.UFUtil (TypecheckUFM, union, find, enterLevel, leaveLevel, currentLevel, initialUFState) where

import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import HsCaml.FrontEnd.Types

type UF = M.Map TypeExpr TypeExpr

data TypecheckState = TypecheckState
  { uf_ :: UF,
    level_ :: Level
  }

initialUFState :: TypecheckState
initialUFState = TypecheckState M.empty 0

type TypecheckUFM = State TypecheckState

-- UnionFind

getUF :: TypecheckUFM UF
getUF = do
  gets uf_

setUF :: UF -> TypecheckUFM ()
setUF uf = do
  TypecheckState _ l <- get
  put $ TypecheckState uf l

setParent :: TypeExpr -> TypeExpr -> TypecheckUFM ()
setParent v p = do
  uf <- getUF
  setUF $ M.insert v p uf

find :: TypeExpr -> TypecheckUFM TypeExpr
find tv = do
  uf <- getUF
  let p = Maybe.fromMaybe tv (uf M.!? tv)
  parent <-
    if p /= tv
      then find p
      else pure p
  setParent tv parent
  pure parent

union :: TypeExpr -> TypeExpr -> TypecheckUFM ()
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

enterLevel :: TypecheckUFM ()
enterLevel = do
  TypecheckState u l <- get
  put $ TypecheckState u (l + 1)

leaveLevel :: TypecheckUFM ()
leaveLevel = do
  TypecheckState u l <- get
  put $ TypecheckState u (l - 1)

currentLevel :: TypecheckUFM Level
currentLevel = do
  gets level_
