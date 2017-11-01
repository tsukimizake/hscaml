{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing, -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module TypeChecker (typeCheck, renameSymsByScope) where
import Control.Lens
import Types
import OCamlType
import Data.Set as S
import qualified Data.Text as T hiding (head) 
import Control.Monad.State.Strict
import Data.Map as M
import Data.Monoid
import Data.Maybe

data TypeRestrict =
    OpType {
        _exp :: InfixOp,
        _theType :: TypeExpr
    }|
    SymType {
        _name :: Name,
        _theType :: TypeExpr
    }|
    TypeEq{
        _names :: [Name]
} deriving (Show, Ord, Eq)

makeLenses ''TypeRestrict

data RenameState =
    RenameState {
        _counter :: Map Name Int,
        _renameStack :: Map Name [Name]
    } deriving (Show)

makeLenses ''RenameState

instance AsTExpr Expr where
    _TExpr  = prism toExpr toTExpr
      where
        toExpr :: TExpr -> Expr
        toExpr (TConstant x _)= Constant x
        toExpr (TVar x _) = Var x
        toExpr (TParen x _) = Paren (toExpr x)
        toExpr (TInfixOpExpr x y z _) = InfixOpExpr (toExpr x) y (toExpr z)
        toExpr (TBegEnd x _) = BegEnd (toExpr x)
        toExpr (TMultiExpr x _) = MultiExpr (fmap toExpr x)
        toExpr (TConstr x _) = Constr (toExpr x)
        toExpr (TIfThenElse x y z _) = IfThenElse (toExpr x) (toExpr y) (toExpr z)
        toExpr (TMatch x y _) = Match (toExpr x) (fmap toExpr <$> y)
        toExpr (TWhile x y _) = While (toExpr x) (toExpr y)
        toExpr (TFunApply x y _) = FunApply x (fmap toExpr y)
        toExpr (TLet x y _) = Let x (toExpr y)
        toExpr (TLetRec x y _) = LetRec x (toExpr y)
        toExpr (TLetIn x y z _) = LetIn x (toExpr y) (toExpr z)
        toExpr (TTypeDecl x y _) = TypeDecl x y
        toTExpr :: Expr -> Either Expr TExpr
        toTExpr e = case typeCheck e of
            Left _ -> Left e
            Right res -> return res


typeCheck :: Expr -> Either CompileError TExpr
typeCheck = Right . initialTypeInfer . renameSymsByScope

-- とりあえずプリミティブや即値やアノテーション書かれたやつにだけ型を付ける
initialTypeInfer :: Expr -> TExpr
initialTypeInfer (Constant x@(IntVal _)) = TConstant x ocamlInt
initialTypeInfer (Constant x@(BoolVal _)) = TConstant x ocamlBool
initialTypeInfer (Var x) = TVar x UnspecifiedType
initialTypeInfer (Paren e) = TParen (initialTypeInfer e) UnspecifiedType
initialTypeInfer (InfixOpExpr e f g) = TInfixOpExpr
                                       (initialTypeInfer e) f (initialTypeInfer g) UnspecifiedType
initialTypeInfer (BegEnd e) = TBegEnd (initialTypeInfer e) UnspecifiedType
initialTypeInfer (MultiExpr e) = TMultiExpr (fmap initialTypeInfer e) UnspecifiedType
initialTypeInfer (Constr e) = TConstr (initialTypeInfer e) UnspecifiedType
initialTypeInfer (IfThenElse e f g) = TIfThenElse (initialTypeInfer e) (initialTypeInfer f) (initialTypeInfer g) UnspecifiedType
initialTypeInfer (Match e f) = TMatch (initialTypeInfer e) (fmap initialTypeInfer <$> f) UnspecifiedType
initialTypeInfer (While e f) = TWhile (initialTypeInfer e) (initialTypeInfer f) UnspecifiedType
initialTypeInfer (FunApply e f) = TFunApply e (fmap initialTypeInfer f) UnspecifiedType
initialTypeInfer (Let e f) = TLet e (initialTypeInfer f) UnspecifiedType
initialTypeInfer (LetRec e f) = TLetRec e (initialTypeInfer f) UnspecifiedType
initialTypeInfer (LetIn e f g) = TLetIn e (initialTypeInfer f) (initialTypeInfer g) UnspecifiedType
initialTypeInfer (TypeDecl e f) = TTypeDecl e f  UnspecifiedType

-- let x = 0
-- in let x = 1
-- in x
-- =>
-- let x0 = 0
-- in let x1 = 1
-- in x1
-- x1が元からあった時のためのgensymは後で


initialRenameState :: RenameState
initialRenameState = RenameState {_counter=M.empty, _renameStack=M.empty}

genSym :: Name -> State RenameState Name
genSym name = do
    oldmap <- use counter
    let n = fromMaybe 0 (oldmap ^. at name) :: Int
    let newmap = oldmap & at name ?~ (n+1)
    counter .= newmap
    pure $ "_" <> name <> "_gen_" <> (T.pack $ show n)


mapMExpr :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
mapMExpr f x@(Constant _) = f x
mapMExpr f x@(Var _) = f x
mapMExpr f (Paren e) = do
    e' <- f e
    pure $ Paren e'
mapMExpr f (InfixOpExpr e iop g) = do
    e' <- f e
    g' <- f g
    pure $ InfixOpExpr e' iop g'
mapMExpr f (BegEnd e) = do
    e' <- f e
    pure $ BegEnd e'
mapMExpr f (MultiExpr e) = do
    e' <- mapM f e
    pure $ MultiExpr e'
mapMExpr f (Constr e) = do
    e' <- f e
    pure $ Constr e'
mapMExpr f (IfThenElse e1 e2 e3) = do
    e1' <- f e1
    e2' <- f e2
    e3' <- f e3
    pure $ IfThenElse e1' e2' e3'
mapMExpr f (Match e1 e2) = do
    e1' <- f e1
    e2' <- mapM (\(x, y) -> do
                     y' <- f y
                     pure (x, y')) e2
    pure $ Match e1' e2'
mapMExpr f (While e1 e2) = do
    e1' <- f e1
    e2' <- f e2
    pure $ While e1' e2'
mapMExpr f (FunApply e1 e2) = do
    e2' <- mapM f e2
    pure $ FunApply e1 e2'
mapMExpr f (Let e1 e2) =  do
    e2' <- f e2
    pure $ Let e1 e2'
mapMExpr f (LetRec e1 e2) =  do
    e2' <- f e2
    pure $ LetRec e1 e2'
mapMExpr f (LetIn e1 e2 e3) = do
    e2' <- f e2
    e3' <- f e3
    pure $ LetIn e1 e2' e3'
mapMExpr _ (TypeDecl e1 e2) = pure $ TypeDecl e1 e2

renameSymsByScope :: Expr -> Expr
renameSymsByScope e = evalState (impl e) initialRenameState

impl :: Expr -> State RenameState Expr
impl (V s) = do
        stack <- use renameStack
        let newname = stack ^. at s & fromJust & head
        pure $ V newname
impl x@(IntC _) = pure $ x
impl (Let (VarPattern t (Sym s)) e) = do
    s' <- pushAndRenameSym s
    e' <- impl e
    popRenameStack s    
    pure (Let (VarPattern t (Sym s')) e')
impl (LetRec (VarPattern t (Sym s)) e) = do
    s' <- pushAndRenameSym s
    e' <- impl e
    popRenameStack s    
    pure (LetRec (VarPattern t (Sym s')) e')
impl (LetIn (VarPattern t (Sym s)) e1 e2) = do
    s' <- pushAndRenameSym s
    e1' <- impl e1
    e2' <- impl e2
    popRenameStack s    
    pure (LetIn (VarPattern t (Sym s')) e1' e2')

impl (Let (FuncPattern t (Sym s) args) e) = do
    s' <- pushAndRenameSym s
    argsText' <- mapM (pushAndRenameSym . unwrapSym) args
    let args' = fmap Sym argsText'
    e' <- impl e
    popRenameStack s
    mapM_ (popRenameStack . unwrapSym) args
    pure $ Let (FuncPattern t (Sym s') (args')) e'
impl (LetRec (FuncPattern t (Sym s) args) e) = do
    s' <- pushAndRenameSym s
    argsText' <- mapM (pushAndRenameSym . unwrapSym) args
    let args' = fmap Sym argsText'
    e' <- impl e
    popRenameStack s
    mapM_ (popRenameStack . unwrapSym) args
    pure $ LetRec (FuncPattern t (Sym s') (args')) e'

impl (LetIn (FuncPattern t (Sym s) args) e1 e2) = do
    s' <- pushAndRenameSym s
    argsText' <- mapM (pushAndRenameSym . unwrapSym) args
    let args' = fmap Sym argsText'
    e1' <- impl e1
    e2' <- impl e2
    popRenameStack s
    mapM_ (popRenameStack . unwrapSym) args 
    pure $ LetIn (FuncPattern t (Sym s') (args')) e1' e2'
impl e = mapMExpr impl e

unwrapSym :: Sym -> Name
unwrapSym s = s ^. Types.name

pushAndRenameSym :: Name -> State RenameState Name
pushAndRenameSym s = do
        s' <- genSym s
        stack <- use renameStack
        let xs = fromMaybe [] (stack ^. at s) :: [Name]
        renameStack .= (stack & at s ?~ (s':xs))
        pure s'

popRenameStack :: Name -> State RenameState ()
popRenameStack s = do
    stack <- use renameStack
    let xs = fromJust (stack ^. at s)
    renameStack .= (stack & at s ?~ tail xs)
    pure ()

-- 最初から型がわかっている式の一覧
initialRestriction :: Set TypeRestrict
initialRestriction = S.fromList
    [
      OpType Mul (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType Plus (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType Minus (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType Div (ocamlInt ::-> ocamlInt ::-> ocamlInt),
      OpType MulDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      OpType PlusDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      OpType MinusDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      OpType DivDot (ocamlFloat ::-> ocamlFloat ::-> ocamlFloat),
      -- ToDo,Compareの型推論が効かない. 多相型を入れる必要があるか
      OpType (Compare LessThan) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType (Compare LessThanEq) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType (Compare GreaterThan) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType (Compare GreaterThanEq) (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),

      OpType BoolAnd (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType BoolOr (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool),
      OpType Mod (UnspecifiedType ::-> UnspecifiedType ::-> ocamlBool)
    ]

typeInfer :: State (Set TypeRestrict) TExpr
typeInfer = undefined

-- まずシンボルのリネームを先にやったほうがよいのでは？
-- 型が付きうるのは、シンボル、演算子、式. 式はTExprに型が付くから、シンボルと演算子だけメモすればよい
-- letの右辺からの推論
-- let f x y = x*y
-- => f : int->int->int
-- => x, y : int

-- if 左辺 is Unspecified and 右辺 is Unspecified
-- 右辺を掘る
-- if 左辺 xor 右辺 is specified
-- specifyしてUnspecifiedだったほうを型情報Mapに入れる
-- if どっちもspecified
-- 整合チェック
-- patternからの推論
-- let (x:int) = a
-- => x : int

-- 多相型入ったらどうなるんだろーねー？

-- 全て型が付いているかどうかのチェック
typedExprisValid :: TExpr -> Either CompileError TExpr
typedExprisValid = undefined



