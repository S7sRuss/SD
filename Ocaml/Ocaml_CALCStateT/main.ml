open Parser
open Syntax

let rec print_list = function 
[] -> print_string "\n";
| e::l -> print_string e ; print_string " " ; print_list l

let rec prompt lexbuf =
	print_string "# " ;
	flush stdout;
  try 
  	let s = Parser.main Lexer.token lexbuf in
			print_endline (unparse_cmd s);
			(** print_string "free: "; print_list (free_cmd s); **)
			(** print_endline (eval_cmd s); **) prompt lexbuf
  with
	| SyntaxError (p1,p2) -> 
			print_string ("Parsing error:"^p1^" to "^p2^"\n");  
			Lexing.flush_input lexbuf; prompt lexbuf; 
			
	| End_of_file -> ()

let main () =
  let lexbuf = Lexing.from_channel (stdin) in prompt lexbuf 
;;

main();;
