type ast = 
    Var of string
  | Num of int
  | Add of ast * ast
  | Fun of string * ast
  | App of ast * ast
;;

type ty = 
    IntTy
  | FunTy of ty * ty
  | VarTy of int * (ty ref)
  | None
;;

type envir = (string * ty) list
;;

let find env x = List.assoc x env
;;

let assoc env x v = (x,v)::env
;;

let typevars = ref 0
;;

let newtype () = 
  typevars := !typevars + 1; VarTy (!typevars,ref None)
;;

let rec unify t t' = 
  match t,t' with 
      
    | VarTy (n,rs), VarTy (n',rs') -> 
	if n = n' then true
	else if !rs = None then (rs := t'; true)
	else unify !rs t'
    
    | VarTy (n,rs), t
    | t, VarTy (n,rs) ->
	if !rs = None then (rs := t ; true)
	else unify !rs t

    | FunTy (s,t), FunTy (s',t') -> unify s s' && unify t t'
    | IntTy, IntTy -> true
    | None, None -> false
    | t,t' -> t = t'
;;

let rec typecheck (e:ast) (env:envir) = 
  match e with
      Var x -> find env x

    | Num n -> IntTy

    | Add (e, e') -> 
	let l = typecheck e env in
	let r = typecheck e' env in
	if unify l IntTy && unify r IntTy then IntTy else None

    | Fun (x, e) -> 
	let s = newtype () in
	let env' = assoc env x s in
	let t = typecheck e env' in
	FunTy (s,t)

    | App (e, e') -> 
	let l = typecheck e env in
	let r = typecheck e' env in
	let s = newtype () in
	if unify l (FunTy (r, s)) then s else None
;;
    
