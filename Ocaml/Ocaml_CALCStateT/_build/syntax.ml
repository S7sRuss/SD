type state = 
	| Number of int 
	| Add of state * state
	| Sub of state * state 
	| Mul of state * state 
	| Div of state * state 
	| Id of string
	| Decl of string * state * state
	| Var of string
	| Deref of state
	| Assign of state * state
	| Free of string
	| If of state * state * state
	| While of state  * state
	| Seq of  state * state
	| Eq of state * state
	| Not of state
	| Call of state * state
	| Fun of string * state

type loc = int

type value = 
	| Num of int
	| Ref of loc
	| Bool of bool

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

let newloc m = 0 

let rec getloc l m =
	match m with
	| (i,v)::t -> if l=i then v else getloc l t
	| [] -> raise (Failure("not found"))

let rec setloc nv l m =
	match m with
	| (i,v)::t -> if i=l then (i,nv)::t else (i,v)::(setloc nv l t)
	| _ -> raise (Failure("not found setloc"))
		

(**funções semânticas **)

let rec unparse a =
	match a with
		| Number n -> "Num("^string_of_int n^")"
		| Add (l,r) -> "Add("^(unparse l)^","^(unparse r)^")"
		| Sub (l,r) -> "Sub("^(unparse l)^","^(unparse r)^")"
		| Mul (l,r) -> "Mul("^(unparse l)^","^(unparse r)^")"
		| Div (l,r) -> "Div("^(unparse l)^","^(unparse r)^")"
		| Id (x) -> x
		| Decl (x,e1,e2) -> "Decl("^x^","^(unparse e1)^","^(unparse e2)^")"
		| Var (x) -> "Var("^x^")"
		| Deref (x) -> " !("^(unparse x)^")"
		| Assign (x,v) -> (unparse x)^":=("^(unparse v)^")"
		| Free (x) -> "Free("^x^")"
		| If (c,i,e) -> "If("^(unparse c)^","^(unparse i)^","^(unparse e)^")"
		| While (c,e) -> "While("^(unparse c)^","^(unparse e)^")"
		| Seq (e1,e2) -> "Seq("^(unparse e1)^","^(unparse e2)^")"
		| Eq (e1,e2) -> "Eq("^(unparse e1)^","^(unparse e2)^")"
		| Not (e1) -> "Not("^(unparse e1)^")"
		| Fun (x,e1) -> "Fun"^x^"->"^(unparse e1)^""
		| Call(e1,e2) -> "Call("^(unparse e1)^","^(unparse e2)^")"


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
		| Var (x) -> []
		| Deref (x) -> []
		| Assign (x,v) -> []
		| Free (x) -> []
		| If (c,i,e) -> []
		| While (c,e) -> []
		| Seq (e1,e2) -> []
		| Eq (e1,e2) -> []
		| Not (e1) -> []
		| Fun(x,e1) -> []
		| Call(e1,e2) -> []

let rec typecheck e env = 
	match e with
		| Number n -> 0
		| Add (l,r) -> 0
		| Sub (l,r) -> 0
		| Mul (l,r) -> 0
		| Div (l,r) -> 0
		| Id (x) -> 0
		| Decl (x,e1,e2) -> 0
		| Var (x) -> 0
		| Deref (x) -> 0
		| Assign (x,v) -> 0
		| Free (x) -> 0
		| If (c,i,e) -> 0
		| While (c,e) -> 0
		| Seq (e1,e2) -> 0
		| Eq (e1,e2) -> 0
		| Not (e1) -> 0
		| Fun(x,e1) -> 0
		| Call(e1,e2) -> 0

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
		| Var (x) -> Num(0)
		| Deref (x) -> Num(0)
		| Assign (x,v) -> Num(0)
		| Free (x) -> Num(0)
		| If (c,i,e) -> Num(0)
		| While (c,e) -> Num(0)
		| Seq (e1,e2) -> Num(0)
		| Eq (e1,e2) -> Num(0)
		| Not (e1) -> Num(0)
		| Fun(x,e1) -> Num(0)
		| Call(e1,e2) -> Num(0)

let rec comp e = 
	match e with
		| Number n -> ["ldc.i4 "^(string_of_int n)]
		| Add (l,r) -> (comp l)@(comp r)@["add"]
		| Sub (l,r) -> (comp l)@(comp r)@["sub"]
		| Mul (l,r) -> (comp l)@(comp r)@["mul"]
		| Div (l,r) -> (comp l)@(comp r)@["div"]
		| Id (x) -> ["Id"]
		| Decl (x,e1,e2) -> ["Decl"]
		| Var (x) -> ["Comp"]
		| Deref (x) -> ["Comp"]
		| Assign (x,v) -> ["Comp"]
		| Free (x) -> ["Comp"]
		| If (c,i,e) -> ["Comp"]
		| While (c,e) -> ["Comp"]
		| Seq (e1,e2) -> ["Comp"]
		| Eq (e1,e2) -> ["Comp"]
		| Not (e1) -> ["Comp"]
		| Fun(x,e1) -> ["Fun"]
		| Call(e1,e2) -> ["Call"]

(** funções para imprimir o resultado das semânticas **)

let rec print_list l =
	match l with
	| a::t -> a^"\n"^(print_list t)
	| [] -> ""

let rec unparse_cmd c = "unparse:"^(unparse c) 

let string_of_result e = 
	match e with
	| Num(n) -> string_of_int n
	| Ref(n) -> "Ref("^(string_of_int n)^")"
	| Bool(b) -> string_of_bool b


let eval_cmd c = "eval:"^(string_of_result (eval c []))

let free_cmd e = free e


