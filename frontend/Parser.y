{
module Parser (exprParser) where
import Types
import Token
import Lexer
import Control.Lens hiding ((:<), (:>))
}

%name parser
%tokentype {Token}
%error {parseError}

%token
let {TokenLet}
rec {TokenRec}
in  {TokenIn}
int {TokenInt $$}
upvar {UpperTokenVar $$}
downvar {DownTokenVar $$}
"+" {TokenPlus}
"-" {TokenMinus}
"*" {TokenMult}
"/" {TokenDiv}
"+." {TokenPlusDot}
"-." {TokenMinusDot}
"*." {TokenMultDot}
"/." {TokenDivDot}
"=" {TokenEq}
"<" {TokenLT}
"<="{TokenLTEq}
">" {TokenGT}
">="{TokenGTEq}
"&&"{TokenLand}
"||"{TokenLor}
"%" {TokenMod}
"(" {TokenOParen}
")" {TokenCParen}
"[" {TokenOBracket}
"]" {TokenCBracket}
"->"{TokenRArrow}
":" {TokenColon}
";" {TokenSemicolon}
"|" {TokenPipe}
"type" {TokenType}

"if" {TokenIf}
"then" {TokenThen}
"else" {TokenElse}
"of" {TokenOf}
%right "="
%left "+" "-" "+." "-."
%left "*" "/" "*." "/."

%%

Expr :: {Expr}                  -- 括弧、演算子優先順位、即値
  : Expr "+" Expr {$1 :+ $3}
  | Expr "-" Expr {$1 :- $3}
  | Expr "*" Expr {$1 :* $3}
  | Expr "/" Expr {$1 :/ $3}
  | Expr "+." Expr {$1 :+. $3}
  | Expr "-." Expr {$1 :-. $3}
  | Expr "*." Expr {$1 :* $3}
  | Expr "/." Expr {$1 :/ $3}
  | Expr "<=" Expr {$1 :<= $3}
  | Expr "<" Expr {$1 :< $3}
  | Expr "=" Expr {$1 :== $3}
  | Expr ">" Expr {$1 :> $3}
  | Expr ">=" Expr {$1 :>= $3}
  | Expr "&&" Expr {$1 :&& $3}
  | Expr "||" Expr {$1 :|| $3}
  | Expr "%" Expr {$1 :% $3}
  | let Pattern "=" Expr {Let $2 $4}
  | let rec Pattern "=" Expr {LetRec $3 $5}
  | Expr "*" Expr {$1 :* $3}
  | Expr "/" Expr {$1 :/ $3}
  | Expr "*." Expr {$1 :*. $3}
  | Expr "/." Expr {$1 :/. $3}
  | downvar ArgExprList {FunApply (Sym $1) $2}
  | "if" Expr "then" Expr "else" Expr {IfThenElse $2 $4 $6}
  | TypeDecl {$1}
  | ArgExpr {$1}

ArgExpr :: {Expr}
  : "(" Expr ")" {Paren $2}
  | int {IntC $1}
  | downvar {V $1}



ArgExprList :: {[Expr]}
  : ArgExpr {[$1]}
  | ArgExpr ArgExprList {$1 : $2}

-- type a = A of int * int
-- type a = A of (int -> int)
-- type a = | A of int | B of int
-- todo: 型引数
TypeDecl :: {Expr}
TypeDecl
  : "type" downvar "=" DataCnstrDecls {TypeDecl $2 $4}

DataCnstrDecls :: {[DataCnstr]}
  : DataCnstrDecl {[$1]}
  | DataCnstrDecl "|" DataCnstrDecls {$1 : $3}

DataCnstrDecl :: {DataCnstr}
  : upvar "of" DataCnstrArgs {DataCnstr $1 (Just $3)}
  | upvar {DataCnstr $1 Nothing}

DataCnstrArgs :: {TypeExpr}
DataCnstrArgs
  : DataCnstrArgs "*" DataCnstrArg {$1 ::* $3}
  | DataCnstrArg {$1}

DataCnstrArg :: {TypeExpr}
DataCnstrArg
  : downvar {TypeAtom $1}
  | DataCnstrArg "->" DataCnstrArg {$1 ::-> $3}

DownvarList :: {[Name]}
  : downvar DownvarList  {$1 : $2}
  | downvar {[$1]}


SymList :: {[Sym]}
  : DownvarList {fmap Sym $1}

Pattern :: {Pattern}
  : downvar {VarPattern Nothing (Sym $1)}
  | Constant {ConstantPattern Nothing $1}
  | "(" Pattern ":" TypeExpr ")" {let theType = Just $4
                                  in ParenPattern theType ($2 & patType .~ theType)}
  | "(" Pattern ")" {ParenPattern Nothing $2}
  | "[" SymList "]" {ListPattern Nothing (fmap Var $2)}
  | downvar SymList {FuncPattern Nothing (Sym $1) $2}
  -- | orpat

Constant :: {Value}
  : int {IntVal $1}

TypeExpr :: {TypeExpr}
  : downvar {TypeAtom $1}
  | TypeExpr "->" TypeExpr {$1 ::-> $3}
  | TypeExpr "*" TypeExpr {$1 ::* $3}
  | TypeExpr "|" TypeExpr {$1 ::+ $3}

{

exprParser :: String -> Expr
exprParser = parser . alexScanTokens

  
parseError tok = error (show tok)


}
