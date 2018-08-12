{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HsCaml.Common.Gensym where
import Control.Lens as L
import Data.Map as M
import Data.Text as T
import Control.Monad.State.Strict
import Data.Maybe

data GensymState =
  GensymState {
  _counter :: Map Text Int,
  _renameStack :: Map Text [Text]
  } deriving (Show)

type GensymM = GensymMT Identity

type GensymMT =  StateT GensymState

runGensymMT :: (Monad m) => GensymMT m a -> m a
runGensymMT impl = evalStateT impl initialGensymState

runGensymM :: GensymM a -> a
runGensymM impl = evalState impl initialGensymState

initialGensymState :: GensymState
initialGensymState = GensymState {_counter=M.empty, _renameStack=M.empty}

L.makeLenses ''GensymState

genSym :: Monad m => Text -> GensymMT m Text
genSym x = do
    oldmap <- use counter
    let n = fromMaybe 0 (oldmap ^. at x) :: Int
    let newmap = oldmap & at x ?~ (n+1)
    counter .= newmap
    pure $ "_" <> x <> "_gen_" <> (T.pack $ show n)
