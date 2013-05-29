type calci = 
	| Number of int 
	| Add of calci * calci
	| Sub of calci * calci 
	| Mul of calci * calci 
	| Div of calci * calci 
	| Id of string
	| Decl of string * calci * calci

type com = 
	| Print of calci 

type binop_type = Op_add | Op_sub | Op_mul | Op_div | Op_lt

let id_counter = ref(0);;

let newid = (incr id_counter) ;; "v"^(string_of_int !id_counter)

exception SyntaxError of string * string

let rec unparse a =
	match a with
		| Number n -> "Num("^string_of_int n^")"
		| Add (l,r) -> "Add("^(unparse l)^","^(unparse r)^")"
		| Sub (l,r) -> "Sub("^(unparse l)^","^(unparse r)^")"
		| Mul (l,r) -> "Mul("^(unparse l)^","^(unparse r)^")"
		| Div (l,r) -> "Div("^(unparse l)^","^(unparse r)^")"
		| Id (x) -> "Id("^x^")"
		| Decl (x,e1,e2) -> "Decl("^x^","^(unparse e1)^","^(unparse e2)^")"


let rec find e env = 
	match env with
		| (x,v)::t -> if x=e then v else find e t
		| _ -> raise (Failure ("Not found: "^e)) 

let rec eval e env = 
	match e with
		| Number n -> n
		| Add (l,r) -> (eval l env)+(eval r env)
		| Sub (l,r) -> (eval l env)-(eval r env)
		| Mul (l,r) -> (eval l env)*(eval r env)
		| Div (l,r) -> (eval l env)/(eval r env)
		| Id (x) -> find x env
		| Decl (x,e1,e2) -> 
			let v = (eval e1 env) in
				let nenv = (x,v)::env in
					eval e2 nenv

let rec comp e = 
	match e with
		| Number n -> ["ldc.i4 "^(string_of_int n)]
		| Add (l,r) -> (comp l)@(comp r)@["add"]
		| Sub (l,r) -> (comp l)@(comp r)@["sub"]
		| Mul (l,r) -> (comp l)@(comp r)@["mul"]
		| Div (l,r) -> (comp l)@(comp r)@["div"]
		| Id (x) -> ["Id"]
		| Decl (x,e1,e2) -> ["Decl"]

let rec print_list l =
	match l with
	| a::t -> a^"\n"^(print_list t)
	| [] -> ""

let rec unparse_cmd c = 
	match c with
		| Print e -> "unparse:"^(unparse e)^"\n"

let eval_cmd c = 
	match c with
	| Print e -> "eval:"^(string_of_int (eval e []))^"\n"


