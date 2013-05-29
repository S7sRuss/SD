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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

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

# 57 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  260 (* SEQ *);
  261 (* SKIP *);
  262 (* IF *);
  263 (* THEN *);
  264 (* ELSE *);
  265 (* WHILE *);
  266 (* DO *);
  267 (* CLPAR *);
  268 (* CRPAR *);
  269 (* FREE *);
  270 (* ASSIGN *);
  271 (* EQUAL *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* MULT *);
  275 (* DIV *);
  276 (* LPAR *);
  277 (* RPAR *);
  278 (* NOT *);
  279 (* DEREF *);
  280 (* DECL *);
  281 (* IN *);
  282 (* FUN *);
  283 (* TYPEOF *);
  284 (* ARROW *);
  285 (* VAR *);
  286 (* BOOLTYPE *);
  287 (* INTTYPE *);
  288 (* NONETYPE *);
  289 (* REFTYPE *);
  290 (* UNITTYPE *);
    0|]

let yytransl_block = [|
  258 (* NAME *);
  259 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\005\000\
\005\000\004\000\004\000\004\000\007\000\007\000\008\000\008\000\
\008\000\009\000\009\000\009\000\010\000\010\000\010\000\010\000\
\006\000\006\000\006\000\006\000\006\000\006\000\011\000\011\000\
\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\001\000\001\000\006\000\004\000\003\000\
\001\000\002\000\003\000\001\000\003\000\001\000\003\000\003\000\
\001\000\003\000\003\000\001\000\004\000\002\000\002\000\001\000\
\006\000\006\000\001\000\003\000\001\000\002\000\001\000\001\000\
\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\029\000\027\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\000\000\
\003\000\004\000\000\000\012\000\000\000\000\000\020\000\000\000\
\000\000\010\000\000\000\022\000\023\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\002\000\011\000\
\000\000\000\000\000\000\000\000\000\000\018\000\019\000\000\000\
\000\000\000\000\007\000\000\000\032\000\031\000\034\000\000\000\
\033\000\000\000\021\000\000\000\000\000\000\000\035\000\000\000\
\008\000\006\000\025\000\026\000"

let yydgoto = "\002\000\
\015\000\057\000\017\000\018\000\058\000\019\000\020\000\021\000\
\022\000\023\000\066\000"

let yysindex = "\009\000\
\065\255\000\000\000\000\000\000\000\000\073\255\073\255\073\255\
\065\255\106\255\106\255\030\255\042\255\065\255\000\000\014\255\
\000\000\000\000\247\254\000\000\012\255\001\255\000\000\007\255\
\006\255\000\000\000\255\000\000\000\000\036\255\025\255\050\255\
\000\000\065\255\073\255\073\255\081\255\081\255\081\255\081\255\
\081\255\040\255\040\255\000\000\073\255\248\254\000\000\000\000\
\037\255\039\255\031\255\001\255\001\255\000\000\000\000\065\255\
\050\255\053\255\000\000\052\255\000\000\000\000\000\000\248\254\
\000\000\051\255\000\000\005\255\040\255\040\255\000\000\040\255\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\189\255\000\000\041\000\208\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\148\255\230\255\253\255\019\000\000\000\000\000\000\000\
\167\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\031\000\251\255\225\255\252\255\000\000\043\000\
\018\000\032\000\017\000"

let yytablesize = 322
let yytable = "\016\000\
\024\000\025\000\026\000\034\000\035\000\028\000\029\000\027\000\
\034\000\001\000\036\000\059\000\032\000\042\000\033\000\043\000\
\073\000\034\000\040\000\041\000\044\000\061\000\062\000\063\000\
\064\000\065\000\037\000\038\000\039\000\048\000\049\000\030\000\
\050\000\050\000\050\000\050\000\050\000\074\000\075\000\060\000\
\076\000\003\000\004\000\031\000\005\000\006\000\038\000\039\000\
\007\000\045\000\056\000\046\000\008\000\034\000\068\000\052\000\
\053\000\067\000\036\000\009\000\069\000\010\000\011\000\012\000\
\047\000\013\000\003\000\004\000\014\000\005\000\006\000\054\000\
\055\000\007\000\003\000\004\000\070\000\008\000\072\000\051\000\
\071\000\000\000\003\000\004\000\009\000\008\000\010\000\011\000\
\012\000\000\000\013\000\000\000\009\000\014\000\010\000\011\000\
\012\000\000\000\013\000\000\000\009\000\014\000\010\000\011\000\
\012\000\000\000\013\000\003\000\004\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\030\000\000\000\
\000\000\012\000\000\000\013\000\030\000\030\000\014\000\030\000\
\000\000\030\000\000\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\000\000\024\000\000\000\030\000\024\000\
\000\000\000\000\024\000\024\000\000\000\024\000\000\000\024\000\
\000\000\024\000\024\000\024\000\024\000\024\000\024\000\009\000\
\024\000\000\000\000\000\000\000\024\000\009\000\009\000\000\000\
\009\000\000\000\009\000\000\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\000\000\024\000\000\000\009\000\
\024\000\000\000\000\000\024\000\024\000\000\000\024\000\000\000\
\024\000\000\000\000\000\024\000\024\000\024\000\024\000\024\000\
\017\000\024\000\000\000\017\000\000\000\024\000\017\000\017\000\
\000\000\017\000\000\000\017\000\000\000\017\000\017\000\017\000\
\017\000\000\000\000\000\017\000\017\000\000\000\013\000\000\000\
\017\000\013\000\000\000\000\000\013\000\013\000\000\000\013\000\
\000\000\013\000\000\000\013\000\013\000\000\000\000\000\013\000\
\013\000\013\000\013\000\000\000\000\000\015\000\013\000\000\000\
\015\000\000\000\000\000\015\000\015\000\000\000\015\000\000\000\
\015\000\000\000\015\000\015\000\015\000\015\000\000\000\000\000\
\015\000\015\000\000\000\016\000\000\000\015\000\016\000\000\000\
\000\000\016\000\016\000\000\000\016\000\000\000\016\000\000\000\
\016\000\016\000\016\000\016\000\000\000\000\000\016\000\016\000\
\000\000\014\000\000\000\016\000\014\000\000\000\000\000\014\000\
\014\000\000\000\014\000\000\000\014\000\000\000\014\000\000\000\
\000\000\000\000\014\000\014\000\014\000\014\000\000\000\000\000\
\000\000\014\000"

let yycheck = "\001\000\
\006\000\007\000\008\000\004\001\014\001\010\000\011\000\009\000\
\004\001\001\000\020\001\043\000\014\000\007\001\001\001\010\001\
\012\001\004\001\018\001\019\001\021\001\030\001\031\001\032\001\
\033\001\034\001\015\001\016\001\017\001\035\000\036\000\002\001\
\037\000\038\000\039\000\040\000\041\000\069\000\070\000\045\000\
\072\000\002\001\003\001\002\001\005\001\006\001\016\001\017\001\
\009\001\014\001\011\001\027\001\013\001\004\001\056\000\038\000\
\039\000\021\001\020\001\020\001\008\001\022\001\023\001\024\001\
\034\000\026\001\002\001\003\001\029\001\005\001\006\001\040\000\
\041\000\009\001\002\001\003\001\025\001\013\001\028\001\037\000\
\064\000\255\255\002\001\003\001\020\001\013\001\022\001\023\001\
\024\001\255\255\026\001\255\255\020\001\029\001\022\001\023\001\
\024\001\255\255\026\001\255\255\020\001\029\001\022\001\023\001\
\024\001\255\255\026\001\002\001\003\001\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\020\001\001\001\255\255\
\255\255\024\001\255\255\026\001\007\001\008\001\029\001\010\001\
\255\255\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\001\001\255\255\025\001\004\001\
\255\255\255\255\007\001\008\001\255\255\010\001\255\255\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\001\001\
\021\001\255\255\255\255\255\255\025\001\007\001\008\001\255\255\
\010\001\255\255\012\001\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\255\255\001\001\255\255\025\001\
\004\001\255\255\255\255\007\001\008\001\255\255\010\001\255\255\
\012\001\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\001\001\021\001\255\255\004\001\255\255\025\001\007\001\008\001\
\255\255\010\001\255\255\012\001\255\255\014\001\015\001\016\001\
\017\001\255\255\255\255\020\001\021\001\255\255\001\001\255\255\
\025\001\004\001\255\255\255\255\007\001\008\001\255\255\010\001\
\255\255\012\001\255\255\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\255\255\255\255\001\001\025\001\255\255\
\004\001\255\255\255\255\007\001\008\001\255\255\010\001\255\255\
\012\001\255\255\014\001\015\001\016\001\017\001\255\255\255\255\
\020\001\021\001\255\255\001\001\255\255\025\001\004\001\255\255\
\255\255\007\001\008\001\255\255\010\001\255\255\012\001\255\255\
\014\001\015\001\016\001\017\001\255\255\255\255\020\001\021\001\
\255\255\001\001\255\255\025\001\004\001\255\255\255\255\007\001\
\008\001\255\255\010\001\255\255\012\001\255\255\014\001\255\255\
\255\255\255\255\018\001\019\001\020\001\021\001\255\255\255\255\
\255\255\025\001"

let yynames_const = "\
  EOL\000\
  SEQ\000\
  SKIP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  CLPAR\000\
  CRPAR\000\
  FREE\000\
  ASSIGN\000\
  EQUAL\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  LPAR\000\
  RPAR\000\
  NOT\000\
  DEREF\000\
  DECL\000\
  IN\000\
  FUN\000\
  TYPEOF\000\
  ARROW\000\
  VAR\000\
  BOOLTYPE\000\
  INTTYPE\000\
  NONETYPE\000\
  REFTYPE\000\
  UNITTYPE\000\
  "

let yynames_block = "\
  NAME\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 41 "parser.mly"
                 ( _1 )
# 290 "parser.ml"
               : Syntax.calcst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statements) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'flowcontrol) in
    Obj.repr(
# 46 "parser.mly"
                               ( Seq(_1,_3) )
# 298 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'flowcontrol) in
    Obj.repr(
# 47 "parser.mly"
               (_1)
# 305 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 51 "parser.mly"
               ( _1 )
# 312 "parser.ml"
               : 'flowcontrol))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
         ( Skip )
# 318 "parser.ml"
               : 'flowcontrol))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                                        ( If (_2,_4,_6,None) )
# 327 "parser.ml"
               : 'flowcontrol))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                              ( While (_2,_4) )
# 335 "parser.ml"
               : 'flowcontrol))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 58 "parser.mly"
                         ( _2 )
# 342 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 59 "parser.mly"
               (_1)
# 349 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 63 "parser.mly"
                   ( Free (_2) )
# 356 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 64 "parser.mly"
                            ( Assign (_1,_3,None) )
# 364 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eq) in
    Obj.repr(
# 65 "parser.mly"
       ( _1 )
# 371 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sum) in
    Obj.repr(
# 69 "parser.mly"
                  ( Eq (_1,_3) )
# 379 "parser.ml"
               : 'eq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sum) in
    Obj.repr(
# 70 "parser.mly"
        ( _1 )
# 386 "parser.ml"
               : 'eq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 74 "parser.mly"
                  ( Add (_1,_3) )
# 394 "parser.ml"
               : 'sum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 75 "parser.mly"
                   ( Sub (_1,_3) )
# 402 "parser.ml"
               : 'sum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 76 "parser.mly"
         ( _1 )
# 409 "parser.ml"
               : 'sum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 80 "parser.mly"
                 ( Mul (_1,_3) )
# 417 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 81 "parser.mly"
                ( Div (_1,_3) )
# 425 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 82 "parser.mly"
       ( _1 )
# 432 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'factor) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 86 "parser.mly"
                                 ( Call (_1,_3) )
# 440 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 87 "parser.mly"
                  ( Not (_2) )
# 447 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 88 "parser.mly"
                  ( Deref (_2,None) )
# 454 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 89 "parser.mly"
            ( _1 )
# 461 "parser.ml"
               : 'op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 93 "parser.mly"
                                         ( Decl (_2,_4,_6,None) )
# 470 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ctype) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 94 "parser.mly"
                                      ( Fun (_2,_4,_6) )
# 479 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
        ( Number(_1) )
# 486 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 96 "parser.mly"
                         ( _2 )
# 493 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
         ( Id(_1,None) )
# 500 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 98 "parser.mly"
                   ( Var(_2,None) )
# 507 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
           ( IntType )
# 513 "parser.ml"
               : 'ctype))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
             ( BoolType )
# 519 "parser.ml"
               : 'ctype))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
            ( UnitType )
# 525 "parser.ml"
               : 'ctype))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
            ( None )
# 531 "parser.ml"
               : 'ctype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ctype) in
    Obj.repr(
# 106 "parser.mly"
                 ( RefType(_2) )
# 538 "parser.ml"
               : 'ctype))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.calcst)
