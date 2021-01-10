{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module HsCaml.Common.Gensym where

import Control.Lens as L
import Control.Monad.State.Strict
import Data.Map as M
import Data.Maybe
import Data.Text as T

data GensymState = GensymState
  { _counter :: Map Text Int,
    _renameStack :: Map Text [Text]
  }
  deriving (Show)

type GensymM = GensymT Identity

type GensymT = StateT GensymState

runGensymT :: (Monad m) => GensymT m a -> m a
runGensymT impl = evalStateT impl initialGensymState

runGensymM :: GensymM a -> a
runGensymM impl = evalState impl initialGensymState

initialGensymState :: GensymState
initialGensymState = GensymState {_counter = M.empty, _renameStack = M.empty}

L.makeLenses ''GensymState

genSym :: Text -> GensymM Text
genSym x = do
  oldmap <- use counter
  let n = fromMaybe 0 (oldmap ^. at x) :: Int
  let newmap = oldmap & at x ?~ (n + 1)
  counter .= newmap
  pure $ "_" <> x <> "_gen_" <> T.pack (show n)
