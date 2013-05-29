type token =
  | EOL
  | INT of (int)
  | STRING of (string)
  | DECL
  | EQ
  | IN
  | END
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LPAR
  | RPAR
  | PRINT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.com
