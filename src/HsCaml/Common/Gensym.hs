{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module HsCaml.Common.Gensym where
import Control.Lens as L
import Data.Map as M
import Data.Text as T
import Control.Monad.State.Strict
import Data.Maybe
import Data.Monoid

data GensymState =
  GensymState {
  _counter :: Map Text Int,
  _renameStack :: Map Text [Text]
  } deriving (Show)

L.makeLenses ''GensymState

type GensymM = State GensymState Text
initialGensymState :: GensymState
initialGensymState = GensymState {_counter=M.empty, _renameStack=M.empty}

genSym :: Text -> State GensymState Text
genSym x = do
    oldmap <- use counter
    let n = fromMaybe 0 (oldmap ^. at x) :: Int
    let newmap = oldmap & at x ?~ (n+1)
    counter .= newmap
    pure $ "_" <> x <> "_gen_" <> (T.pack $ show n)
