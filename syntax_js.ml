(* javascript the good parts *)

let VAR_STATEMENTS = ( "var " NAME ( '=' EXPRESSION )? ( ',' NAME ( '=' EXPRESSION )? )* )*

let NAME = [a-zA-Z][a-zA-Z0-9\_]*

let STRING = '"'[a-zA-Z0-9\_]*'"'

let STATEMENTS = ( EXPRESSION_STATEMENT ';' | DISRUPTE_STATEMENT | IF_STATEMENT | SWITCH_STATEMENT | WHILE_STATEMENT | FOR_STATEMENT | DO_STATEMENT )*

let DISRUPTE_STATEMENT = BREAK_STATEMENT | RETURN_STATEMENT

let BLOCK = '{' STATEMENTS? '}'

let IF_STATEMENT = "if" '(' EXPRESSION ')' BLOCK "else" BLOCK 

let SWITCH_STATEMENT = "switch" '(' EXPRESSION ')' '{' CASE_CAUSE ( (*TODO*)) '}'

let CASE_CAUSE = ( "case" EXPRESSION ':' )+ STATEMENTS

let WHILE_STATEMENT = "while" '(' EXPRESSION ')' BLOCK

let FOR_STATEMENT = "for" '(' ( EXPRESSION_STATEMENT? ';' EXPRESSION? ';' EXPRESSION_STATEMENT | NAME "in" EXPRESSION ) ')' BLOCK 

let DO_STATEMENT = "do" BLOCK "while" '(' EXPRESSION ')' ';'

let RETURN_STATEMENT = "return" EXPRESSION? ';'

let BREAK_STATEMENT = "break" NAME? ';'

let EXPRESSION_STATEMENT = NAME ((** TODO **)) | "del" EXPRESSION REFINEMENT

let EXPRESSION = 
      LITERAL | NAME | '(' EXPRESSION ')' | PREFIX_OPERATOR EXPRESSION | EXPRESSION SUB_EXPRESSION
    | "new" EXPRESSION INVOCATION | "del" EXPRESSION REFINEMENT

(* sub_expression facilita a leitura(auxílio) *)
let SUB_EXPRESSION = INFIX_OPERATOR EXPRESSION | INVOCATION | REFINEMENT | '?' EXPRESSION ':' EXPRESSION

let PREFIX_OPERATOR = "typeof" | '+' | '-' | '!'

let INFIX_OPERATOR = '|' | '&' | "==" | "<>" | ">=" | "<=" | '>' | '<' | '+' | '-' | '*' | '/' | '%'

let INVOCATION = '(' EXPRESSION ')'

let REFINEMENT = ( '.' NAME ) | ( '[' EXPRESSION ']' )

let LITERAL = NUMBER_LITERAL | STRING_LITERAL | OBJECT_LITERAL | ARRAY_LITERAL | FUNCTION_LITERAL

let OBJECT_LITERAL = '{' OBJECT_ELEMENT ( ',' OBJECT_ELEMENT )* '}'	

let OBJECT_ELEMENT =  (NAME | STRING) ':' EXPRESSION

let PARAMETERS = '(' (NAME (',' NAME)*)? ')'

let FUNCTION_LITERAL = "function" (NAME)? PARAMETERS FUNCTION_BODY	

let FUNCTION_BODY = '{' VAR_STATEMENTS STATEMENTS '}'
