{
module Parser (parseStatement, parseExpr) where
import Types
import Token
import Lexer
import Control.Lens hiding ((:<), (:>))
}

%name stmtParser Stmt
%name exprParser Expr
%tokentype {Token}
%error {parseError}

%token
int {TokenInt $$}
upvar {UpperTokenVar $$}
downvar {DownTokenVar $$}
"let" {TokenLet}
"rec" {TokenRec}
"in"  {TokenIn}
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
"true" {TokenTrue}
"false" {TokenFalse}
"match" {TokenMatch}
"with" {TokenWith}
";;" {TokenDoubleSemicolon}
%right "="
%left "+" "-" "+." "-."
%left "*" "/" "*." "/."

%%
Stmt :: {Statement}
  : Exprs {Statement $1}

Exprs :: {[Expr]}
  : Expr ";;" {[$1]}
  | Expr ";;" Exprs  {$1 : $3}

Expr :: {Expr}
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
  | "let" Pattern "=" Expr {Let $2 $4}
  | "let" "rec" Pattern "=" Expr {LetRec $3 $5}
  | "let" Pattern "=" Expr "in" Expr {LetIn $2 $4 $6}
  | Expr "*" Expr {$1 :* $3}
  | Expr "/" Expr {$1 :/ $3}
  | Expr "*." Expr {$1 :*. $3}
  | Expr "/." Expr {$1 :/. $3}
  | downvar ArgExprList {FunApply (Sym $1) $2}
  | "if" Expr "then" Expr "else" Expr {IfThenElse $2 $4 $6}
  | "match" Expr "with" MatchPats {Match $2 $4}
  | TypeDecl {$1}
  | ArgExpr {$1}

MatchPats :: {[(Pattern, Expr)]}
  : MatchPat {[$1]}
  | MatchPat MatchPats {$1 : $2}
MatchPat :: {(Pattern, Expr)}
  : "|" Pattern "->" Expr {($2, $4)}

ArgExpr :: {Expr}
  : "(" Expr ")" {Paren $2}
  | downvar {V $1}
  | Constant {Constant $1}

Constant :: {Value}
  : int {IntVal $1}
  | "true" {BoolVal True}
  | "false" {BoolVal False}

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
  : upvar "of" DataCnstrArgs {DataCnstr $1 $3}
  | upvar {DataCnstr $1 []}

DataCnstrArgs :: {[TypeExpr]}
DataCnstrArgs
  : DataCnstrArgs "*" DataCnstrArg {$1 ++ [$3]}
  | DataCnstrArg {[$1]}

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
  : downvar {VarPattern UnspecifiedType (Sym $1)}

  | Constant {ConstantPattern UnspecifiedType $1}
  | "(" Pattern ":" TypeExpr ")" {let theType = $4
                                  in ParenPattern theType ($2 & _patType .~ theType)}
  | "(" Pattern ")" {ParenPattern UnspecifiedType $2}
  | "[" SymList "]" {ListPattern UnspecifiedType (fmap Var $2)}
  | downvar SymList {FuncPattern UnspecifiedType (Sym $1) $2}
  -- | orpat


TypeExpr :: {TypeExpr}
  : downvar {TypeAtom $1}
  | TypeExpr "->" TypeExpr {$1 ::-> $3}
  | "(" TypeExpr ")" {ParenTypeExpr $2}
  | TypeExpr "*" TypeExpr {$1 ::* $3}
  | TypeExpr "|" TypeExpr {$1 ::+ $3}

{

parseStatement :: String -> Statement
parseStatement = stmtParser . alexScanTokens

parseExpr :: String -> Expr
parseExpr = exprParser . alexScanTokens
  
parseError tok = error (show tok)


}
