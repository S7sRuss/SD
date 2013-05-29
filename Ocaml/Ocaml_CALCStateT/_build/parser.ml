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

# 50 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  260 (* DECL *);
  261 (* EQ *);
  262 (* IN *);
  263 (* END *);
  264 (* FUN *);
  265 (* IS *);
  266 (* IF *);
  267 (* THEN *);
  268 (* ELSE *);
  269 (* WHILE *);
  270 (* DEREF *);
  271 (* SEQ *);
  272 (* EQUAL *);
  273 (* NOT *);
  274 (* VAR *);
  275 (* FREE *);
  276 (* ASSIGN *);
  277 (* PLUS *);
  278 (* MINUS *);
  279 (* MULT *);
  280 (* DIV *);
  281 (* LPAR *);
  282 (* RPAR *);
  283 (* PRINT *);
    0|]

let yytransl_block = [|
  258 (* INT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\005\000\005\000\005\000\006\000\006\000\006\000\007\000\007\000\
\007\000\007\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\005\000\001\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\004\000\002\000\
\002\000\001\000\007\000\005\000\003\000\004\000\002\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\024\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\003\000\
\006\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
\000\000\017\000\016\000\000\000\023\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\002\000\000\000\000\000\000\000\
\012\000\013\000\000\000\000\000\000\000\000\000\000\000\022\000\
\015\000\000\000\020\000\000\000\000\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000"

let yysindex = "\013\000\
\107\255\000\000\000\000\000\000\016\255\035\255\125\255\049\255\
\133\255\125\255\051\255\055\255\125\255\000\000\002\255\000\000\
\000\000\247\254\238\254\000\000\058\255\075\255\077\255\084\255\
\125\255\000\000\000\000\095\255\000\000\015\255\000\000\107\255\
\125\255\125\255\125\255\125\255\125\255\133\255\125\255\107\255\
\107\255\082\255\093\255\000\000\000\000\021\255\238\254\238\254\
\000\000\000\000\096\255\041\255\123\255\038\255\107\255\000\000\
\000\000\107\255\000\000\107\255\088\255\124\255\088\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\090\255\033\255\000\000\009\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\255\066\255\078\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\063\255\000\000\111\255\000\000"

let yygindex = "\000\000\
\000\000\219\255\225\255\001\000\245\255\031\000\070\000\247\255"

let yytablesize = 158
let yytable = "\026\000\
\045\000\030\000\031\000\054\000\036\000\037\000\033\000\024\000\
\053\000\018\000\027\000\034\000\035\000\001\000\018\000\018\000\
\032\000\061\000\022\000\018\000\018\000\046\000\063\000\018\000\
\018\000\042\000\062\000\052\000\051\000\018\000\018\000\018\000\
\018\000\011\000\018\000\034\000\035\000\023\000\011\000\011\000\
\044\000\034\000\035\000\011\000\011\000\007\000\058\000\011\000\
\011\000\060\000\007\000\007\000\032\000\011\000\011\000\007\000\
\007\000\029\000\011\000\007\000\007\000\034\000\035\000\005\000\
\047\000\048\000\009\000\007\000\007\000\005\000\007\000\009\000\
\009\000\025\000\005\000\028\000\009\000\009\000\010\000\039\000\
\009\000\009\000\038\000\010\000\010\000\040\000\009\000\009\000\
\010\000\010\000\008\000\009\000\010\000\010\000\041\000\008\000\
\008\000\043\000\010\000\010\000\008\000\008\000\032\000\010\000\
\008\000\049\000\050\000\055\000\003\000\004\000\005\000\004\000\
\008\000\008\000\006\000\008\000\007\000\004\000\056\000\008\000\
\009\000\057\000\004\000\010\000\011\000\012\000\003\000\004\000\
\005\000\059\000\064\000\013\000\006\000\000\000\003\000\004\000\
\005\000\000\000\009\000\000\000\006\000\010\000\011\000\012\000\
\000\000\000\000\000\000\000\000\000\000\013\000\011\000\012\000\
\000\000\000\000\000\000\000\000\000\000\013\000"

let yycheck = "\009\000\
\032\000\013\000\001\001\041\000\023\001\024\001\016\001\007\000\
\040\000\001\001\010\000\021\001\022\001\001\000\006\001\007\001\
\015\001\055\000\003\001\011\001\012\001\033\000\060\000\015\001\
\016\001\025\000\058\000\039\000\038\000\021\001\022\001\023\001\
\024\001\001\001\026\001\021\001\022\001\003\001\006\001\007\001\
\026\001\021\001\022\001\011\001\012\001\001\001\006\001\015\001\
\016\001\012\001\006\001\007\001\015\001\021\001\022\001\011\001\
\012\001\003\001\026\001\015\001\016\001\021\001\022\001\001\001\
\034\000\035\000\001\001\023\001\024\001\007\001\026\001\006\001\
\007\001\025\001\012\001\025\001\011\001\012\001\001\001\005\001\
\015\001\016\001\025\001\006\001\007\001\009\001\021\001\022\001\
\011\001\012\001\001\001\026\001\015\001\016\001\011\001\006\001\
\007\001\003\001\021\001\022\001\011\001\012\001\015\001\026\001\
\015\001\036\000\037\000\026\001\002\001\003\001\004\001\001\001\
\023\001\024\001\008\001\026\001\010\001\007\001\026\001\013\001\
\014\001\026\001\012\001\017\001\018\001\019\001\002\001\003\001\
\004\001\007\001\007\001\025\001\008\001\255\255\002\001\003\001\
\004\001\255\255\014\001\255\255\008\001\017\001\018\001\019\001\
\255\255\255\255\255\255\255\255\255\255\025\001\018\001\019\001\
\255\255\255\255\255\255\255\255\255\255\025\001"

let yynames_const = "\
  EOL\000\
  DECL\000\
  EQ\000\
  IN\000\
  END\000\
  FUN\000\
  IS\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DEREF\000\
  SEQ\000\
  EQUAL\000\
  NOT\000\
  VAR\000\
  FREE\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  LPAR\000\
  RPAR\000\
  PRINT\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sequence) in
    Obj.repr(
# 36 "parser.mly"
              ( _1 )
# 217 "parser.ml"
               : Syntax.state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sequence) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 40 "parser.mly"
                    ( Seq(_1,_3) )
# 225 "parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 41 "parser.mly"
       ( _1 )
# 232 "parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'equality) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'sequence) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'sequence) in
    Obj.repr(
# 45 "parser.mly"
                                          ( If(_2,_4,_6) )
# 241 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'equality) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sequence) in
    Obj.repr(
# 46 "parser.mly"
                                    ( While(_3,_5) )
# 249 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equality) in
    Obj.repr(
# 47 "parser.mly"
           ( _1 )
# 256 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                  ( Eq(_1,_3))
# 264 "parser.ml"
               : 'equality))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
       (_1)
# 271 "parser.ml"
               : 'equality))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
                 ( Add(_1,_3) )
# 279 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
                  ( Sub(_1,_3) )
# 287 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
       ( _1 )
# 294 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 62 "parser.mly"
                 ( Mul(_1,_3) )
# 302 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 63 "parser.mly"
                ( Div(_1,_3) )
# 310 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 64 "parser.mly"
       ( _1 )
# 317 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fact) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'fact) in
    Obj.repr(
# 68 "parser.mly"
                      ( Call(_1,_3) )
# 325 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'equality) in
    Obj.repr(
# 69 "parser.mly"
               ( Not(_2) )
# 332 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 70 "parser.mly"
             ( Deref(_2) )
# 339 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 71 "parser.mly"
       ( _1 )
# 346 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 75 "parser.mly"
                                  (Decl(_2,_4,_6))
# 355 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 76 "parser.mly"
                         ( Fun(_2,_4))
# 363 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                 ( _2 )
# 370 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "parser.mly"
                       ( Var(_3) )
# 377 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
              ( Free (_2) )
# 384 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 80 "parser.mly"
      ( Number(_1) )
# 391 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
         (Id(_1))
# 398 "parser.ml"
               : 'fact))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.state)
;;
# 84 "parser.mly"

# 425 "parser.ml"
