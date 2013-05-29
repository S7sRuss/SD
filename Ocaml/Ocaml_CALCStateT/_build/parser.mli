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
  | IF
  | THEN
  | ELSE
  | WHILE
  | DEREF
  | SEQ
  | EQUAL
  | NOT
  | VAR
  | FREE
  | ASSIGN
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LPAR
  | RPAR
  | PRINT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.state
