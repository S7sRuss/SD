type state = 
	| Number of int 
	| Add of state * state
	| Sub of state * state 
	| Mul of state * state 
	| Div of state * state 
	| Id of string
	| Decl of string * state * state
	| Var of state
	| Deref of state
	| Assign of state * state
	| Free of state
	| If of state * state * state
	| While of state  * state
	| Seq of  state * state
	| Eq of state * state
	| Not of state

type loc = int

type value = 
	| Num of int
	| Ref of loc
	| Boolean of bool

type com = 
	| Print of state 

type binop_type = Op_add | Op_sub | Op_mul | Op_div | Op_lt

let id_counter = ref(0);;

let newid = (incr id_counter) ;; "v"^(string_of_int !id_counter)

exception SyntaxError of string * string


(** Operações de cast **)

let toNum e = 
	match e with
	| Num(n) -> n 
	| _ -> raise (Failure("Not a number"))


(** funções de manipulação do ambiente **)

let rec find e env = 
	match env with
		| (x,v)::t -> if x=e then v else find e t
		| _ -> raise (Failure ("Not found: "^e)) 


let assoc x v env = (x,v)::env

(** funções de manipulação da memória **)

let newloc m =
	let l = length m in
		

(**funções semânticas **)

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


(** usando ambientes **)
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
		| Fun (x,e) -> Abs(x,e)
		| Call (e1,e2) -> 
			let t = eval e1 env in 
				match t with
				| Abs(x,e3) -> 
					let v= eval e2 env in
						let nenv = (x,v)::env in
							eval e3 nenv
				| _ -> raise (Failure ("not a function"))

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


(** funções para imprimir o resultado das semânticas **)

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
	| Abs(x,e) -> "Abs("^x^","^(unparse e)^")"


let eval_cmd c = 
	match c with
	| Print e -> "eval:"^(string_of_result (eval e []))

let free_cmd e = 
	match e with
	| Print c -> free c


