type token =
  | EOL
  | INT of (int)
  | STRING of (string)
  | DECL
  | EQ
  | IN
  | END
  | FUN
  | IS
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LPAR
  | RPAR
  | PRINT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.com
