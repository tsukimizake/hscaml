{
{-# LANGUAGE NoMonomorphismRestriction, CPP, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module HsCaml.Parser.Lexer (alexScanTokens)

where
import Prelude
import Data.Text hiding (map, take)
import HsCaml.Parser.Token
}

%wrapper "basic"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$uppercase = [A-Z]
$downcase = [a-z]

tokens :-
  $white+                               ;
  "(*".*"*)"                            ;
  let { tok TokenLet}
  in  { tok TokenIn}
  $digit+  {tokRead TokenInt}
  [\+]  {tok TokenPlus}
  [\-]  {tok TokenMinus}
  [\*]  {tok TokenMult}
  [\/]  {tok TokenDiv}
  [=]   {tok TokenEq}
  "+."  {tok TokenPlusDot}
  "-."  {tok TokenMinusDot}
  "*."  {tok TokenMultDot}
  "/."  {tok TokenDivDot}
  [\(]  {tok TokenOParen}
  [\)]  {tok TokenCParen}
  [\[]  {tok TokenOBracket}
  [\]]  {tok TokenCBracket}
  [&]   {tok TokenAnd}
  [\<] {tok TokenLT}
  [\>] {tok TokenGT}
  "<=" {tok TokenLTEq}
  ">=" {tok TokenGTEq}
  ":"  {tok TokenColon}
  ";"  {tok TokenSemicolon}
  ";;"  {tok TokenDoubleSemicolon}
  "->" {tok TokenRArrow}
  "|"  {tok TokenPipe}
  "[|" {tok TokenOPipeBracket}
  "|]" {tok TokenCPipeBracket}
  as     {tok TokenAs}
  assert     {tok TokenAssert}
  asr     {tok TokenAsr}
  begin     {tok TokenBegin}
  class     {tok TokenClass}
  closed     {tok TokenClosed}
  constraint {tok TokenConstraint}
  do     {tok TokenDo}
  done     {tok TokenDone}
  downto     {tok TokenDownto}
  else     {tok TokenElse}
  end     {tok TokenEnd}
  excetion     {tok TokenException}
  external     {tok TokenExternal}
  false     {tok TokenFalse}
  for     {tok TokenFor}
  fun     {tok TokenFun}
  function     {tok TokenFunction}
  functor     {tok TokenFunctor}
  if     {tok TokenIf}
  include     {tok TokenInclude}
  inherit     {tok TokenInherit}
  land     {tok TokenLand}
  lazy     {tok TokenLazy}
  lor     {tok TokenLor}
  lsl     {tok TokenLsl}
  lsr     {tok TokenLsr}
  lxor     {tok TokenLxor}
  match     {tok TokenMatch}
  method     {tok TokenMethod}
  mod     {tok TokenMod}
  module     {tok TokenModule}
  mutable     {tok TokenMutable}
  new     {tok TokenNew}
  of     {tok TokenOf}
  open     {tok TokenOpen}
  or     {tok TokenOr}
  parser     {tok TokenParser}
  private     {tok TokenPrivate}
  rec     {tok TokenRec}
  sig     {tok TokenSig}
  struct     {tok TokenStruct}
  then     {tok TokenThen}
  to     {tok TokenTo}
  true     {tok TokenTrue}
  try     {tok TokenTry}
  type     {tok TokenType}
  val     {tok TokenVal}
  virtual     {tok TokenVirtual}
  when     {tok TokenWhen}
  while     {tok TokenWhile}
  with     {tok TokenWith}
  plus     {tok TokenPlus}
  minus     {tok TokenMinus}
  times     {tok TokenMult}
  div     {tok TokenDiv}
  eq     {tok TokenEq}

-- {tok TokenEOF}
$uppercase [$alpha $digit \_ \']*         { tokText   UpperTokenVar }
$downcase  [$alpha $digit \_ \']*         { tokText   DownTokenVar }
' [$alpha $digit \_ \']*         { tokText   QuotedTokenVar }

{

tok :: Token -> String -> Token
tok cls s =  cls

tokText :: (Text -> Token) -> String -> Token
tokText cls s = cls (pack s)

tokRead :: (Read a) => (a->Token) -> String -> Token
tokRead cls s = cls (read s)


}
