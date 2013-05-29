{

module Lexer where

}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  $digit+				{ \s -> TokenInt (read s) }
  \+  					{ \_ -> TokenPlus }
  \-  					{ \_ -> TokenMinus }
  \*  					{ \_ -> TokenMul }
  \/  					{ \_ -> TokenDiv }
  \(  					{ \_ -> TokenLParen }
  \)  					{ \_ -> TokenRParen }
  $alpha [$alpha $digit \_ \']*		{ \s -> TokenSym s }



{

data Token = 
       	 TokenInt Int
       | TokenSym String
       | TokenPlus
       | TokenMinus
       | TokenMul
       | TokenDiv
       | TokenLParen
       | TokenRParen
       deriving (Eq, Show )

}
