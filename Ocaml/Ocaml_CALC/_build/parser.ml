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

# 33 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* MULT *);
  263 (* DIV *);
  264 (* LPAR *);
  265 (* RPAR *);
  266 (* PRINT *);
    0|]

let yytransl_block = [|
  258 (* INT *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\004\000\004\000\
\004\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\003\000\001\000\002\000\003\000\003\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\010\000\000\000\
\000\000\000\000\009\000\001\000\006\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\007\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\010\000\011\000"

let yysindex = "\006\000\
\247\254\000\000\014\255\000\000\010\255\017\255\000\000\014\255\
\021\255\022\255\000\000\000\000\000\000\015\255\013\255\013\255\
\013\255\013\255\000\000\022\255\022\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\255\255\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\255\008\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\024\000\015\000\016\000"

let yytablesize = 34
let yytable = "\005\000\
\003\000\003\000\005\000\005\000\003\000\003\000\001\000\005\000\
\004\000\003\000\012\000\004\000\004\000\006\000\007\000\007\000\
\004\000\013\000\015\000\016\000\008\000\008\000\000\000\019\000\
\015\000\016\000\002\000\017\000\018\000\020\000\021\000\014\000\
\022\000\023\000"

let yycheck = "\001\001\
\010\001\001\001\004\001\005\001\004\001\005\001\001\000\009\001\
\001\001\009\001\001\001\004\001\005\001\000\001\002\001\002\001\
\009\001\001\001\004\001\005\001\008\001\008\001\255\255\009\001\
\004\001\005\001\001\001\006\001\007\001\015\000\016\000\008\000\
\017\000\018\000"

let yynames_const = "\
  EOL\000\
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
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'command) in
    Obj.repr(
# 33 "parser.mly"
             ( _1 )
# 117 "parser.ml"
               : Syntax.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
            ( Print(_2) )
# 124 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 41 "parser.mly"
                ( Add(_1,_3) )
# 132 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 42 "parser.mly"
                  ( Sub(_1,_3) )
# 140 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "parser.mly"
       ( _1 )
# 147 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
            ( raiseError() )
# 153 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 48 "parser.mly"
                 ( Mul(_1,_3) )
# 161 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 49 "parser.mly"
                ( Div(_1,_3) )
# 169 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 50 "parser.mly"
       ( _1 )
# 176 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
     ( Number(_1) )
# 183 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                 ( _2 )
# 190 "parser.ml"
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
# 58 "parser.mly"

# 217 "parser.ml"
