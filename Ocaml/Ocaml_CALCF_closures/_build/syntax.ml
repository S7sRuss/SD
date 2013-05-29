type calcf = 
	| Number of int 
	| Add of calcf * calcf
	| Sub of calcf * calcf 
	| Mul of calcf * calcf 
	| Div of calcf * calcf 
	| Id of string
	| Decl of string * calcf * calcf
	| Fun of string * calcf
	| Call of calcf * calcf

type value = 
	| Num of int
	| Closure of string * calcf * (string * value) list


type com = 
	| Print of calcf 

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
		| Fun (x,e) -> "Fun("^x^","^(unparse e)^")"
		| Call (e1,e2) -> "Call("^(unparse e1)^","^(unparse e2)^")"


(** este free pode devolver duplicados, mas não deve haver problema com isso **)
let rec free e =
	match e with
		| Number n -> []
		| Add (l,r) -> (free l)@(free r)
		| Sub (l,r) -> (free l)@(free r)
		| Mul (l,r) -> (free l)@(free r)
		| Div (l,r) -> (free l)@(free r)
		| Id (x) -> [x]	
		| Decl (x,e1,e2) -> (free e1)@(List.filter (fun a -> a <> x) (free e2))
		| Fun (x,e) ->  List.filter (fun a -> a <> x) (free e)
		| Call (e1,e2) -> (free e1)@(free e2)

let toNum e = 
	match e with
	| Num(n) -> n 
	| _ -> raise (Failure("Not a number"))


let rec find e env = 
	match env with
		| (x,v)::t -> if x=e then v else find e t
		| _ -> raise (Failure ("Not found: "^e)) 

(** usando closures **)
let rec eval e env = 
	match e with
		| Number n -> Num(n)
		| Add (l,r) -> Num(toNum((eval l env))+toNum((eval r env)))
		| Sub (l,r) -> Num(toNum((eval l env))-toNum((eval r env)))
		| Mul (l,r) -> Num(toNum((eval l env))*toNum((eval r env)))
		| Div (l,r) -> let v = toNum((eval r env)) in if v=0 then raise (Failure ("Division by Zero")) else Num(toNum((eval l env))/v)
		| Id (x) -> find x env
		| Decl (x,e1,e2) ->
			let v = eval e1 env in
				let nenv = (x,v)::env in
					eval e2 nenv
		| Fun (x,e1) -> Closure(x,e1,env)
		| Call (e1,e2) -> 
			let f = eval e1 env in
				let v = eval e2 env in 
					match f with
					| Closure(x,e3,env1) ->
						let nenv = (x,v)::env1 in
							eval e3 nenv
					| _ -> raise (Failure("Not a Closure"))



let rec comp e = 
	match e with
		| Number n -> ["ldc.i4 "^(string_of_int n)]
		| Add (l,r) -> (comp l)@(comp r)@["add"]
		| Sub (l,r) -> (comp l)@(comp r)@["sub"]
		| Mul (l,r) -> (comp l)@(comp r)@["mul"]
		| Div (l,r) -> (comp l)@(comp r)@["div"]
		| Id (x) -> ["Id"]
		| Decl (x,e1,e2) -> ["Decl"]
		| Fun (x,e1) -> ["Fun"]
		| Call (e1,e2) -> ["Call"]

let rec print_list l =
	match l with
	| a::t -> a^"\n"^(print_list t)
	| [] -> ""

let rec unparse_cmd c = 
	match c with
		| Print e -> "unparse:"^(unparse e)

let string_of_result e = 
	match e with
	| Num(n) -> string_of_int n
	| Closure(x,e1,env) -> "Closure("^x^","^(unparse e1)^",env)"


let eval_cmd c = 
	match c with
	| Print e -> "eval:"^(string_of_result (eval e []))

let free_cmd e = 
	match e with
	| Print c -> free c


