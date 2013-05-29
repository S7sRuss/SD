%{
open Syntax

let parse_error s = 
  print_endline s;
  flush stdout
	
let string_of_position p =
	(string_of_int p.Lexing.pos_lnum) ^":" ^ (string_of_int p.Lexing.pos_cnum)

let raiseError () = 
	let p1 = (Parsing.rhs_start_pos 1) in 
  let p2 = (Parsing.symbol_end_pos()) in
	Parsing.clear_parser ();
  raise (SyntaxError(string_of_position p1, string_of_position p2))

%}


%token EOL
%token <int> INT
%token <string> STRING
%token DECL EQ IN END FUN IS
%token IF THEN ELSE WHILE
%token DEREF SEQ EQUAL NOT VAR FREE ASSIGN
%token PLUS MINUS MULT DIV LPAR RPAR 
%token PRINT 

%start main

%type <Syntax.state> main

%% /* Grammar rules */

main: 
	sequence EOL { $1 }
;

sequence:
  sequence SEQ stmt { Seq($1,$3) }
| stmt { $1 }
;

stmt:
  IF equality THEN sequence ELSE sequence { If($2,$4,$6) }
| WHILE LPAR equality RPAR sequence { While($3,$5) }
| equality { $1 }
;

equality:
  expr EQUAL expr { Eq($1,$3)}
| expr {$1}
;

expr:
  expr PLUS term { Add($1,$3) }
| expr MINUS term { Sub($1,$3) }
| term { $1 }
;

term: 
| term MULT call { Mul($1,$3) }
| term DIV call { Div($1,$3) }
| call { $1 }
;

call:
  fact LPAR fact RPAR { Call($1,$3) }
| NOT equality { Not($2) }
| DEREF fact { Deref($2) }
| fact { $1 }
;

fact:
  DECL STRING EQ expr IN stmt END {Decl($2,$4,$6)} 
| FUN STRING IS stmt END { Fun($2,$4)}
| LPAR expr RPAR { $2 }
| VAR LPAR STRING RPAR { Var($3) }
| FREE STRING { Free ($2) }  
| INT { Number($1) }
| STRING {Id($1)}
;

%%
