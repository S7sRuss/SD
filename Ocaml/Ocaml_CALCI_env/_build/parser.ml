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

# 37 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  260 (* DECL *);
  261 (* EQ *);
  262 (* IN *);
  263 (* END *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* MULT *);
  267 (* DIV *);
  268 (* LPAR *);
  269 (* RPAR *);
  270 (* PRINT *);
    0|]

let yytransl_block = [|
  258 (* INT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\004\000\004\000\
\004\000\004\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\001\000\002\000\007\000\003\000\
\003\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\011\000\013\000\000\000\000\000\014\000\
\000\000\000\000\000\000\010\000\006\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\008\000\009\000\000\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\012\000"

let yysindex = "\002\000\
\030\255\000\000\019\255\000\000\000\000\020\255\030\255\000\000\
\042\255\003\255\040\255\000\000\000\000\036\255\031\255\000\000\
\033\255\033\255\026\255\026\255\030\255\000\000\040\255\040\255\
\000\000\000\000\252\254\030\255\039\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\255\000\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\255\018\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\249\255\035\000\036\000"

let yytablesize = 56
let yytable = "\015\000\
\005\000\028\000\001\000\017\000\018\000\005\000\005\000\005\000\
\005\000\003\000\017\000\018\000\005\000\027\000\003\000\003\000\
\003\000\003\000\004\000\013\000\029\000\003\000\014\000\004\000\
\004\000\004\000\004\000\004\000\005\000\003\000\004\000\004\000\
\005\000\006\000\004\000\005\000\006\000\007\000\017\000\018\000\
\021\000\007\000\016\000\022\000\007\000\030\000\017\000\018\000\
\002\000\019\000\020\000\023\000\024\000\000\000\025\000\026\000"

let yycheck = "\007\000\
\001\001\006\001\001\000\008\001\009\001\006\001\007\001\008\001\
\009\001\001\001\008\001\009\001\013\001\021\000\006\001\007\001\
\008\001\009\001\001\001\001\001\028\000\013\001\003\001\006\001\
\007\001\008\001\009\001\002\001\003\001\000\001\013\001\002\001\
\003\001\004\001\002\001\003\001\004\001\012\001\008\001\009\001\
\005\001\012\001\001\001\013\001\012\001\007\001\008\001\009\001\
\001\001\010\001\011\001\017\000\018\000\255\255\019\000\020\000"

let yynames_const = "\
  EOL\000\
  DECL\000\
  EQ\000\
  IN\000\
  END\000\
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
# 34 "parser.mly"
             ( _1 )
# 136 "parser.ml"
               : Syntax.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
      ( Print(_1) )
# 143 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 42 "parser.mly"
                ( Add(_1,_3) )
# 151 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "parser.mly"
                  ( Sub(_1,_3) )
# 159 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "parser.mly"
       ( _1 )
# 166 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
            ( raiseError() )
# 172 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                                 (Decl(_2,_4,_6))
# 181 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 50 "parser.mly"
                 ( Mul(_1,_3) )
# 189 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 51 "parser.mly"
                ( Div(_1,_3) )
# 197 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 52 "parser.mly"
       ( _1 )
# 204 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "parser.mly"
     ( Number(_1) )
# 211 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                 ( _2 )
# 218 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
         (Id(_1))
# 225 "parser.ml"
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
# 61 "parser.mly"

# 252 "parser.ml"
