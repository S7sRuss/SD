{
open Parser

exception UnknownToken of char
}

let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z']

let whitespace = (' '| '\t' | '\r' | '\n' | ("//" [^'\n' '\r']*) | ("/*"[^'/']*"*/"))+

rule token = parse
	| whitespace { token lexbuf }
	| '+' { PLUS } 
	| '-' { MINUS } 
	| '*' { MULT } 
	| '/' { DIV } 
	| '(' { LPAR }
	| ')' { RPAR }
	| '=' { EQ }
	| '!' { DEREF }
	| ';' { SEQ }
	| "==" { EQUAL }
	| "->" {IS}
	| "not" { NOT }
	| "decl" { DECL }
	| "in" { IN }
	| "end" { END }
	| ":=" { ASSIGN }
	| "var" { VAR }
	| "free" { FREE }
	| ";;" { EOL }
	| "if" {IF}
	| "then" {THEN}
	| "else" {ELSE}
	| "while" {WHILE}
	| "fun" {FUN}
	| digit+ as num { INT (int_of_string num) }
	| char (char | digit)* as word { STRING word }
	| _ as tk { raise (UnknownToken tk) }
	| eof { raise End_of_file }