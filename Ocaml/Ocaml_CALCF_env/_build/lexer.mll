{
open Parser

exception UnknownToken of char
}

let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z']

rule token = parse
	| [' ' '\t' '\r' '\n' ] { token lexbuf }
	| ';' { EOL }
	| '+' { PLUS } 
	| '-' { MINUS } 
	| '*' { MULT } 
	| '/' { DIV } 
	| '(' { LPAR }
	| ')' { RPAR }
	| '=' { EQ }
	| "decl" { DECL }
	| "in" { IN }
	| "end" { END }
	| "fun" { FUN }
	| "->" { IS }
	| digit+ as num { INT (int_of_string num) }
	| char (char | digit)* as word { STRING word }
	| _ as tk { raise (UnknownToken tk) }
	| eof { raise End_of_file }