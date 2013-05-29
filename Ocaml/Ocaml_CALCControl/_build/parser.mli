type token =
  | EOL
  | NAME of (string)
  | INT of (int)
  | SEQ
  | SKIP
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | CLPAR
  | CRPAR
  | FREE
  | ASSIGN
  | EQUAL
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LPAR
  | RPAR
  | NOT
  | DEREF
  | DECL
  | IN
  | FUN
  | TYPEOF
  | ARROW
  | VAR
  | BOOLTYPE
  | INTTYPE
  | NONETYPE
  | REFTYPE
  | UNITTYPE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.calcst
