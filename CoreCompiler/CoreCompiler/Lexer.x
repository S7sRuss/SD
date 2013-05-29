{

module Lexer where

}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$nl    = [\n\r]                 -- 

tokens :-

  $nl+                                  ; -- not skipping...
  $white+				;
  \;\;                                  { \_ -> TokenEnd }
  $digit+				{ \s -> TokenNum (read s) }
  \->					{ \_ -> TokenArrow }
  \+  					{ \_ -> TokenPlus }
  \-  					{ \_ -> TokenMinus }
  \*  					{ \_ -> TokenMul }
  \/  					{ \_ -> TokenDiv }
  \(  					{ \_ -> TokenLParen }
  \)  					{ \_ -> TokenRParen }
  =>					{ \_ -> TokenDArrow }
  =					{ \_ -> TokenEq }
  decl 					{ \_ -> TokenDecl }
  in 					{ \_ -> TokenIn }
  fun 					{ \_ -> TokenFun }
  int 					{ \_ -> TokenInt }
  bool 					{ \_ -> TokenBool }
  ref 					{ \_ -> TokenRef }
  unit 					{ \_ -> TokenUnit }
  \:=                                   { \_ -> TokenAssign }
  \:                                    { \_ -> TokenColon }
  var                                   { \_ -> TokenVar }
  \!                                    { \_ -> TokenDeref }
  free                                  { \_ -> TokenFree }
  if                                    { \_ -> TokenIf }
  then                                  { \_ -> TokenThen } 
  else                                  { \_ -> TokenElse }
  while                                 { \_ -> TokenWhile }
  do                                    { \_ -> TokenDo }
  not                                   { \_ -> TokenNot }
  \;                                    { \_ -> TokenSeq }
  $alpha [$alpha $digit \_ \']*		{ \s -> TokenId s }



{

data Token = 
       	 TokenNum Int
       | TokenPlus
       | TokenMinus
       | TokenMul
       | TokenDiv
       | TokenLParen
       | TokenRParen
       | TokenDecl
       | TokenIn
       | TokenEq
       | TokenId String
       | TokenArrow
       | TokenDArrow
       | TokenFun
       | TokenInt
       | TokenBool
       | TokenRef 
       | TokenUnit
       | TokenColon
       | TokenVar
       | TokenDeref
       | TokenAssign
       | TokenFree
       | TokenIf
       | TokenThen
       | TokenElse
       | TokenNot
       | TokenWhile
       | TokenDo
       | TokenSeq	
       | TokenEnd
       deriving (Eq, Show )

}
