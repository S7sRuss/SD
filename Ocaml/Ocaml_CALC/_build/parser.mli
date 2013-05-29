type token =
  | EOL
  | INT of (int)
  | ID of (string)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LPAR
  | RPAR
  | PRINT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.com
