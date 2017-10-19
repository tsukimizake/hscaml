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
'+' {TokenPlus}
'-' {TokenMinus}
'*' {TokenMult}
'/' {TokenDiv}
'+.' {TokenPlusDot}
'-.' {TokenMinusDot}
'*.' {TokenMultDot}
'/.' {TokenDivDot}
'=' {TokenEq}
'<' {TokenLT}
'<='{TokenLTEq}
'>' {TokenGT}
'>='{TokenGTEq}
'&&'{TokenLand}
'||'{TokenLor}
'&' {TokenAnd}
'|' {TokenOr}
'%' {TokenMod}
'(' {TokenOParen}
')' {TokenCParen}
'[' {TokenOBracket}
']' {TokenCBracket}
'->'{TokenRArrow}
':' {TokenColon}
if {TokenIf}
then {TokenThen}
else {TokenElse}

%right '='
%left '+' '-' '+.' '-.'
%left '*' '/' '*.' '/.'

%%

Expr :: {Expr}                  -- 括弧、演算子優先順位、即値
  : Expr '+' Expr {$1 :+ $3}
  | Expr '-' Expr {$1 :- $3}
  | Expr '*' Expr {$1 :* $3}
  | Expr '/' Expr {$1 :/ $3}
  | Expr '+.' Expr {$1 :+. $3}
  | Expr '-.' Expr {$1 :-. $3}
  | Expr '*.' Expr {$1 :* $3}
  | Expr '/.' Expr {$1 :/ $3}
  | Expr '<=' Expr {$1 :<= $3}
  | Expr '<' Expr {$1 :< $3}
  | Expr '=' Expr {$1 :== $3}
  | Expr '>' Expr {$1 :> $3}
  | Expr '>=' Expr {$1 :>= $3}
  | Expr '&&' Expr {$1 :&& $3}
  | Expr '||' Expr {$1 :|| $3}
  | Expr '&' Expr {$1 :& $3}
  | Expr '|' Expr {$1 :| $3}
  | Expr '%' Expr {$1 :% $3}
  | let Pattern '=' Expr {Let $2 $4}
  | let rec Pattern '=' Expr {LetRec $3 $5}
  | Expr '*' Expr {$1 :* $3}
  | Expr '/' Expr {$1 :/ $3}
  | Expr '*.' Expr {$1 :*. $3}
  | Expr '/.' Expr {$1 :/. $3}
  | downvar ArgExprList {FunApply (Sym $1) $2}
  | if Expr then Expr else Expr {IfThenElse $2 $4 $6}
  | ArgExpr {$1}

ArgExpr :: {Expr}
  : '(' Expr ')' {Paren $2}
  | int {IntC $1}
  | downvar {V $1}


ArgExprList :: {[Expr]}
  : ArgExpr {[$1]}
  | ArgExpr ArgExprList {$1 : $2}
SymList :: {[Sym]}
  : downvar SymList  {Sym $1 : $2}
  | downvar {[Sym $1]}

Pattern :: {Pattern}
  : downvar {VarPattern Nothing (Sym $1)}
  | Constant {ConstantPattern Nothing $1}
  | '(' Pattern ':' TypeExpr ')' {let theType = Just $4
                                  in ParenPattern theType ($2 & patType .~ theType)}
  | '(' Pattern ')' {ParenPattern Nothing $2}
  | '[' SymList ']' {ListPattern Nothing (fmap Var $2)}
  | downvar SymList {FuncPattern Nothing (Sym $1) $2}
  -- | orpat

Constant :: {Value}
  : int {IntVal $1}

TypeExpr :: {TypeExpr}
  : upvar {TypeAtom $1}
  | TypeExpr '->' TypeExpr {$1 ::-> $3}
  | TypeExpr '*' TypeExpr {$1 ::* $3}
  | TypeExpr '+' TypeExpr {$1 ::+ $3}

{

exprParser :: String -> Expr
exprParser = parser . alexScanTokens

  
parseError tok = error (show tok)


}
