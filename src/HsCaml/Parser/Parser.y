{
{-# LANGUAGE NoStrict, NoStrictData #-}
module HsCaml.Parser.Parser (parseStatement, parseExpr, parseTopLevel) where
import HsCaml.FrontEnd.Types as Types
import HsCaml.Parser.Token
import HsCaml.Parser.Lexer
import qualified Data.Text as T
}

%name stmtParser Stmt
%name exprParser Expr
%name topLevelParser TopLevel
%tokentype {Token}
%error {parseError}

%token
int {TokenInt $$}
upvar {UpperTokenVar $$}
downvar {DownTokenVar $$}
qvar {QuotedTokenVar $$}
"let" {TokenLet}
"rec" {TokenRec}
"fun" {TokenFun}
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
"begin" {TokenBegin}
"end" {TokenEnd}
";;" {TokenDoubleSemicolon}
"[|" {TokenOPipeBracket}
"|]" {TokenCPipeBracket}

%right "="
%left "+" "-" "+." "-."
%left "*" "/" "*." "/."

%%
Stmt :: {Statement}
  : TopLevels {Statement $1}

TopLevels :: {[TopLevel]}
  : TopLevel ";;" {[$1]}
  | TopLevel ";;" TopLevels  {$1 : $3}

TopLevel :: {TopLevel}
  : Expr {TopLevelExpr $1}
  | TypeDecl {TopLevelTypeDecl $1}

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
  | "let" LetPattern "=" Expr {Let $2 $4}
  | "let" "rec" LetPattern "=" Expr {LetRec $3 $5}
  | "let" LetPattern "=" Expr "in" Expr {LetIn $2 $4 $6}
  | "let" "rec" LetPattern "=" Expr "in" Expr {LetRecIn $3 $5 $7}
  | "fun" SymList "->" Expr {LetIn (FuncLetPattern UnspecifiedType (Sym (T.pack "fun")) (zip $2 (repeat UnspecifiedType))) $4 $4}
  | Expr "*" Expr {$1 :* $3}
  | Expr "/" Expr {$1 :/ $3}
  | Expr "*." Expr {$1 :*. $3}
  | Expr "/." Expr {$1 :/. $3}
  | Expr ArgExprList {FunApply $1 $2}
  | "if" Expr "then" Expr "else" Expr {IfThenElse $2 $4 $6}
  | "match" Expr "with" MatchPats {Match $2 $4}
  | ArgExpr {$1}
  | ExprList {MultiExpr $1}
  | "[" ExprList "]" {Types.List $2}
  | "[|" ExprList "|]" {Array $2}

ExprList :: {[Expr]}
  : Expr ";" {[$1]}
  | Expr ";" ExprList {$1 : $3}

MatchPats :: {[(Pattern, Expr)]}
  : MatchPat {[$1]}
  | MatchPat MatchPats {$1 : $2}
MatchPat :: {(Pattern, Expr)}
  : "|" Pattern "->" Expr {($2, $4)}

ArgExpr :: {Expr}
  : "(" Expr ")" {Paren $2}
  | "begin" Expr "end" {BegEnd $2}
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
-- type a = A of int | B of int
-- type 'a list = Nil | Cons 'a 'a list
TypeDecl :: {TypeDecl}
TypeDecl
  : "type" downvar "=" DataCnstrDecls {TypeDecl $2 $4}
  | "type" TypeArgs downvar "=" DataCnstrDecls  {TypeDecl $3 $5}

TypeArgs :: {[TypeExpr]}
  : qvar {[TypeVar $1 0]}
  | qvar TypeArgs {TypeVar $1 0 : $2}

TypeApplication :: {TypeExpr}
TypeApplication
  : TypeArgs downvar {TypeApplication $1 (TypeAtom $2)}

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
  | qvar {TypeVar $1 0}
  | TypeApplication {$1}
  | DataCnstrArg "->" DataCnstrArg {$1 ::-> $3}

DownvarList :: {[Name]}
  : downvar DownvarList  {$1 : $2}
  | downvar {[$1]}


SymList :: {[Sym]}
  : DownvarList {fmap Sym $1}

Pattern :: {Pattern}
  : Constant {ConstantPattern UnspecifiedType $1}
  | downvar {VarPattern UnspecifiedType (Sym $1)}
  | "(" Pattern ":" TypeExpr ")" {let theType = $4
                                  in ParenPattern theType ($2{ mpatType = theType })}
  | "(" Pattern ")" {ParenPattern UnspecifiedType $2}
  | "[" PatternList "]" {ListPattern UnspecifiedType $2}
  -- | orpat

PatternList :: {[Pattern]}
  : Pattern {[$1]}
  | Pattern PatternList {$1 : $2}

LetPattern :: {LetPattern}
  : Pattern {LetPattern UnspecifiedType $1}
  | downvar SymList {FuncLetPattern UnspecifiedType (Sym $1) (zip $2 (repeat UnspecifiedType))}



TypeExpr :: {TypeExpr}
  : downvar {TypeAtom $1}
  | qvar {TypeVar $1 0}
  | TypeExpr "->" TypeExpr {$1 ::-> $3}
  | "(" TypeExpr ")" {ParenTypeExpr $2}
  | TypeExpr "*" TypeExpr {$1 ::* $3}
  | TypeExpr "|" TypeExpr {$1 ::+ $3}

{

parseStatement :: String -> Statement
parseStatement = stmtParser . alexScanTokens

parseExpr :: String -> Expr
parseExpr = exprParser . alexScanTokens

parseTopLevel :: String -> TopLevel
parseTopLevel = topLevelParser . alexScanTokens

parseError tok = error (show tok)


}
