{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    int  { TokenInt $$ }
    '+'  { TokenPlus }
    '-'  { TokenMinus }
    '*'  { TokenMul }
    '/'  { TokenDiv }
    '('  { TokenLParen }
    ')'  { TokenRParen }

%%

Exp : 
       Exp '+' Term                      { Add $1 $3 }
     | Exp '-' Term                      { Sub $1 $3 }
     | Term                              { $1 }

Term :
       Term '*' Factor                   { Mul $1 $3 }
     | Term '/' Factor                   { Div $1 $3 }
     | Factor                            { $1 }

Factor :
       int                                { Num $1 }
     | '(' Exp ')'                        { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
