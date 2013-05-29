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
%token DECL EQ IN END
%token PLUS MINUS MULT DIV LPAR RPAR 
%token PRINT 

%start main

%type <Syntax.com> main

%% /* Grammar rules */

main: 
	command EOL { $1 }
;
 
command:
	expr { Print($1) }
;

expr:
	expr PLUS term { Add($1,$3) }
| expr MINUS term { Sub($1,$3) }
| term { $1 }
| error EOL { raiseError() }
;

term: 
	DECL STRING EQ expr IN expr END {Decl($2,$4,$6)} 
| term MULT fact { Mul($1,$3) }
| term DIV fact { Div($1,$3) }
| fact { $1 }
;

fact:
	INT { Number($1) }
| LPAR expr RPAR { $2 }
| STRING {Id($1)}
;

%%
