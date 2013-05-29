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

# 39 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  260 (* DECL *);
  261 (* EQ *);
  262 (* IN *);
  263 (* END *);
  264 (* FUN *);
  265 (* IS *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* MULT *);
  269 (* DIV *);
  270 (* LPAR *);
  271 (* RPAR *);
  272 (* PRINT *);
    0|]

let yytransl_block = [|
  258 (* INT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\004\000\004\000\
\004\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\001\000\002\000\003\000\003\000\
\001\000\004\000\001\000\007\000\005\000\001\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\014\000\016\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\009\000\000\000\006\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\007\000\008\000\000\000\
\000\000\000\000\010\000\000\000\013\000\000\000\012\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\013\000\014\000"

let yysindex = "\001\000\
\019\255\000\000\004\255\000\000\000\000\006\255\011\255\019\255\
\000\000\023\255\057\255\058\255\000\000\012\255\000\000\015\255\
\020\255\249\254\000\000\052\255\052\255\052\255\052\255\019\255\
\019\255\019\255\000\000\058\255\058\255\000\000\000\000\048\255\
\034\255\040\255\000\000\019\255\000\000\054\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\255\024\255\000\000\000\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\255\042\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\248\255\052\000\053\000\000\000"

let yytablesize = 76
let yytable = "\018\000\
\011\000\001\000\020\000\021\000\015\000\011\000\011\000\027\000\
\016\000\011\000\011\000\011\000\011\000\017\000\011\000\032\000\
\033\000\034\000\003\000\025\000\004\000\005\000\006\000\019\000\
\005\000\024\000\007\000\038\000\026\000\005\000\005\000\003\000\
\008\000\005\000\005\000\002\000\003\000\003\000\005\000\036\000\
\003\000\003\000\004\000\020\000\021\000\003\000\037\000\004\000\
\004\000\020\000\021\000\004\000\004\000\004\000\005\000\006\000\
\004\000\020\000\021\000\007\000\039\000\000\000\035\000\020\000\
\021\000\008\000\020\000\021\000\000\000\022\000\023\000\028\000\
\029\000\000\000\030\000\031\000"

let yycheck = "\008\000\
\001\001\001\000\010\001\011\001\001\001\006\001\007\001\015\001\
\003\001\010\001\011\001\012\001\013\001\003\001\015\001\024\000\
\025\000\026\000\000\001\005\001\002\001\003\001\004\001\001\001\
\001\001\014\001\008\001\036\000\009\001\006\001\007\001\001\001\
\014\001\010\001\011\001\001\001\006\001\007\001\015\001\006\001\
\010\001\011\001\001\001\010\001\011\001\015\001\007\001\006\001\
\007\001\010\001\011\001\010\001\011\001\002\001\003\001\004\001\
\015\001\010\001\011\001\008\001\007\001\255\255\015\001\010\001\
\011\001\014\001\010\001\011\001\255\255\012\001\013\001\020\000\
\021\000\255\255\022\000\023\000"

let yynames_const = "\
  EOL\000\
  DECL\000\
  EQ\000\
  IN\000\
  END\000\
  FUN\000\
  IS\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'command) in
    Obj.repr(
# 35 "parser.mly"
             ( _1 )
# 153 "parser.ml"
               : Syntax.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
      ( Print(_1) )
# 160 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "parser.mly"
                 ( Add(_1,_3) )
# 168 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "parser.mly"
                  ( Sub(_1,_3) )
# 176 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 45 "parser.mly"
       ( _1 )
# 183 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
            ( raiseError() )
# 189 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 50 "parser.mly"
                 ( Mul(_1,_3) )
# 197 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 51 "parser.mly"
                ( Div(_1,_3) )
# 205 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 52 "parser.mly"
       ( _1 )
# 212 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fact) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                       ( Call(_1,_3) )
# 220 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 57 "parser.mly"
        ( _1 )
# 227 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                  (Decl(_2,_4,_6))
# 236 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                         ( Fun(_2,_4))
# 244 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
      ( Number(_1) )
# 251 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                 ( _2 )
# 258 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
         (Id(_1))
# 265 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.com)
;;
# 68 "parser.mly"

# 292 "parser.ml"
