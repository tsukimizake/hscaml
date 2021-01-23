module HsCaml.TypeChecker.TypeCheckEff where

import Control.Monad.State.Strict as S
import qualified Data.Extensible as E
import qualified Data.Extensible.Effect as E
import Data.Map as M
import Data.Proxy
import Data.Text
import qualified HsCaml.Common.Gensym as GS
import HsCaml.FrontEnd.Types
import qualified HsCaml.TypeChecker.UFUtil as UF

type TypecheckEff = E.Eff '["gs" E.>: GS.GensymM, "err" E.>: E.EitherEff CompileError, "uf" E.>: UF.TypecheckUFM, "lv" E.>: E.State (M.Map Sym Level)]

liftUF :: UF.TypecheckUFM a -> TypecheckEff a
liftUF = E.liftEff (Proxy @"uf")

find :: TypeExpr -> TypecheckEff TypeExpr
find tv = liftUF $ UF.find tv

union :: TypeExpr -> TypeExpr -> TypecheckEff ()
union l r = liftUF $ UF.union l r

enterLevel :: TypecheckEff ()
enterLevel = liftUF UF.enterLevel

leaveLevel :: TypecheckEff ()
leaveLevel = liftUF UF.leaveLevel

currentLevel :: TypecheckEff Level
currentLevel = liftUF UF.currentLevel

liftGS :: GS.GensymM a -> TypecheckEff a
liftGS = E.liftEff (Proxy @"gs")

genSym :: Text -> TypecheckEff Text
genSym = liftGS . GS.genSym

throw :: CompileError -> TypecheckEff a
throw = E.throwEff (Proxy @"err")

runTypecheckEff :: TypecheckEff a -> Either CompileError a
runTypecheckEff = E.leaveEff . flip E.evalStateEff M.empty . flip E.evalStateEff UF.initialUFState . E.runEitherEff . flip E.evalStateEff GS.initialGensymState

putLevelMap :: Sym -> Level -> TypecheckEff ()
putLevelMap k v = E.liftEff (Proxy @"lv") . S.modify $ M.insert k v

getLevelMap :: Sym -> TypecheckEff Level
getLevelMap k = E.liftEff (Proxy @"lv") . S.gets $ flip (M.!) k
