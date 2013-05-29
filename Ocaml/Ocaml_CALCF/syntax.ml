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

type result = 
	| Num of int
	| Abs of string * calcf
	| Error of string


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


(** função de substituição, sem avoid-capturing **)
let rec subst x e v =
	match e with
		| Number n -> Number(n)
		| Add (l,r) -> Add((subst x l v),(subst x r v))
		| Sub (l,r) -> Sub((subst x l v),(subst x r v))
		| Mul (l,r) -> Mul((subst x l v),(subst x r v))
		| Div (l,r) -> Div((subst x l v),(subst x r v))
		| Id (t) -> if x=t then v else Id(t)
		| Decl (t,e1,e2) -> if x=t then Decl(x,(subst x e1 v),e2) else Decl(t,(subst x e1 v),(subst x e2 v)) 
		| Fun (t,e1) ->  if x==t then Fun(t,e1) else Fun(t,(subst x e1 v))
		| Call (e1,e2) -> Call((subst x e1 v),(subst x e2 v))

let toNum e = 
	match e with
	| Num(n) -> n 
	| _ -> raise (Failure("Not a number"))

let rec eval e = 
	match e with
		| Number n -> Num(n)
		| Add (l,r) -> Num(toNum((eval l))+toNum((eval r)))
		| Sub (l,r) -> Num(toNum((eval l))-toNum((eval r)))
		| Mul (l,r) -> Num(toNum((eval l))*toNum((eval r)))
		| Div (l,r) -> let v = toNum((eval r)) in if v=0 then Error("Division by Zero") else Num(toNum((eval l))/v)
		| Id (x) -> Error("Id("^x^")")
		| Decl (x,e1,e2) -> eval (subst x e2 e1)
		| Fun (x,e) -> Abs(x,e)
		| Call (e1,e2) -> 
			let t = eval(e1) in 
				match t with
				| Abs(x,e3) -> eval(subst x e3 e2)
				| _ -> Error("not a function")


let rec find e env = 
	match env with
		| (x,v)::t -> if x=e then v else find e t
		| _ -> raise (Failure ("Not found: "^e)) 

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
	| Abs(x,e) -> "Abs("^x^","^(unparse e)^")"
	| Error(x) -> "Error("^x^")"


let eval_cmd c = 
	match c with
	| Print e -> "eval:"^(string_of_result (eval e))

let free_cmd e = 
	match e with
	| Print c -> free c


