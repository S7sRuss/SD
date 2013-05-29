{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    num  { TokenNum $$ }
    '+'  { TokenPlus }
    '-'  { TokenMinus }
    '*'  { TokenMul }
    '/'  { TokenDiv }
    '('  { TokenLParen }
    ')'  { TokenRParen }
    decl { TokenDecl }
    in   { TokenIn }
    '='  { TokenEq }
    id   { TokenId $$ }
    Int  { TokenInt }
    Bool { TokenBool }
    Ref  { TokenRef }
    Unit { TokenUnit }
    fun  { TokenFun }
    '->' { TokenArrow }
    '=>' { TokenDArrow }
    ':'  { TokenColon }
    '!'  { TokenDeref }
    ':=' { TokenAssign }
    free { TokenFree }
    var  { TokenVar }
    if   { TokenIf }
    then { TokenThen }
    else { TokenElse }
    not  { TokenNot }
    while { TokenWhile }
    do    { TokenDo }
    ';'   { TokenSeq }
    ';;'  { TokenEnd }
%%

Start:
       Def ';;'                          { $1 }
;

Def : 
       Asg                               { $1 }
     | Asg ';' Def                       { Seq $1 $3 }
     | decl id '=' Def in Def            { Decl $2 $4 $6 None }
     | fun id ':' Ty '->' Def            { Fun $2 $4 $6 None }
     | if Def then Def else Def          { If $2 $4 $6 None }
     | while Def do Def                  { While $2 $4 }

Asg : 
       BExp                              { $1 }
     | BExp ':=' Asg                     { Assign $1 $3 None }
;


Ty:
       BTy                               { $1 }
     | BTy '=>' Ty                       { FunType $1 $3 }
;

BTy:
       Int                               { IntType }
     | Bool                              { BoolType }
     | Ref '(' Ty ')'                    { RefType $3 }
     | Unit                              { UnitType }
;

BExp:
       Exp                               { $1 }
     | BExp '=' Exp                      { Equal $1 $3 }

Exp : 
       Exp '+' Term                      { Add $1 $3 }
     | Exp '-' Term                      { Sub $1 $3 }
     | Term                              { $1 }
;

Term :
       Term '*' Call                     { Mul $1 $3 }
     | Term '/' Call                     { Div $1 $3 }
     | Call                              { $1 }
;

Call:
       DerefNot                         { $1 }
     | Call DerefNot                    { Call $1 $2 None } 
;

DerefNot:
       Factor                            { $1 }
     | '!' Factor                        { Deref $2 None }
     | not Factor                      { Not $2 }
;

Factor :
       num                                { Num $1 }
     | '(' Def ')'                        { $2 }
     | id                                 { Id $1 None }
     | var '(' Def ')'                    { Var $3 None }
     | free '(' Def ')'                   { Free $3 }
;

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
